//! markdown_view_leptos — Render Markdown at compile time with inline Leptos components
//!
//! This crate provides a single procedural macro:
//!
//! - `markdown_view!`: Converts a string literal or `file = "..."`/`url = "..."` into
//!   a Leptos `view!` tree at compile time.
//!
//! The macro allows embedding
//! real Leptos components inline using a lightweight MDX-like syntax:
//!
//! - Embed components with: `{{ <MyComponent prop=value/> }}`
//! - Include file content: `markdown_view!(file = "README.md")`
//! - Fetch remote content (build-time): `markdown_view!(url = "https://...")`
//!
//! Notes and caveats:
//! - The macro injects generated HTML into the DOM via `inner_html`. Avoid
//!   untrusted Markdown if you need strict sanitization.
//! - Remote `url = "..."` fetch happens at compile time and is disabled under
//!   rust-analyzer to keep IDEs responsive. Prefer `file = "..."` for stability.
//! - Component parsing looks for `{{ ... }}` and treats the inner content as
//!   a Rust/RSX snippet. We intentionally keep this flexible and resilient: if
//!   parsing fails, the content is rendered as plain Markdown to avoid breaking
//!   your build.
//!
//! Example
//!
//! ```ignore
//! use leptos::prelude::*;
//! use markdown_view_leptos::markdown_view;
//!
//! #[component]
//! fn Counter() -> impl IntoView { let (n, set_n) = signal(0); view! { <button on:click=move |_| set_n.update(|v| *v+=1)>{n}</button> } }
//!
//! #[component]
//! pub fn App() -> impl IntoView {
//!     view! { <main>{markdown_view!(r#"Hello {{ <Counter/> }}!"#)}</main> }
//! }
//! ```

use proc_macro::TokenStream;
// no extra items from proc_macro needed beyond TokenStream
use proc_macro2::TokenStream as TokenStream2;
use pulldown_cmark::{html, Options, Parser};
use quote::quote;
use std::fs;
use std::path::PathBuf;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Ident, Lit, LitStr, Token,
};

fn convert_markdown_to_html(markdown: &str) -> String {
    if markdown.trim().is_empty() {
        return String::new();
    }
    let mut options = Options::empty();
    options.insert(Options::ENABLE_TABLES);
    options.insert(Options::ENABLE_FOOTNOTES);
    options.insert(Options::ENABLE_STRIKETHROUGH);
    options.insert(Options::ENABLE_TASKLISTS);
    let parser = Parser::new_ext(markdown, options);
    let mut html_output = String::new();
    html::push_html(&mut html_output, parser);
    html_output
}

/// A parsed piece of the input stream.
enum Segment {
    /// Plain Markdown, to be converted to HTML and injected as `inner_html`.
    Markdown(String),
    /// Inline Leptos component snippet inside `{{ ... }}`.
    Component(TokenStream2),
}

/// Splits the input into Markdown and inline-component segments.
///
/// - Inline components are enclosed by `{{` and `}}` and can span multiple
///   lines. They are parsed as Rust/RSX tokens.
/// - Content between triple backtick fences (```lang) is treated as plain
///   Markdown; any `{{`/`}}` inside code fences are ignored.
fn split_markdown_with_components(input: &str) -> Vec<Segment> {
    let mut segments: Vec<Segment> = Vec::new();
    let bytes = input.as_bytes();

    let mut i: usize = 0; // current cursor
    let mut last: usize = 0; // start of the current Markdown span
    let mut in_fence = false; // inside ``` fenced block

    while i < bytes.len() {
        // Toggle code fence state when encountering "```" at any position.
        if !in_fence
            && i + 2 < bytes.len()
            && bytes[i] == b'`'
            && bytes[i + 1] == b'`'
            && bytes[i + 2] == b'`'
        {
            i += 3;
            in_fence = true;
            continue;
        } else if in_fence
            && i + 2 < bytes.len()
            && bytes[i] == b'`'
            && bytes[i + 1] == b'`'
            && bytes[i + 2] == b'`'
        {
            i += 3;
            in_fence = false;
            continue;
        }

        // Only consider component delimiters outside code fences.
        if !in_fence && i + 1 < bytes.len() && bytes[i] == b'{' && bytes[i + 1] == b'{' {
            let comp_start = i + 2; // skip opening "{{"
            let mut j = comp_start;
            let mut found = None;
            while j + 1 < bytes.len() {
                // Don't toggle code fences while inside component scanning.
                if bytes[j] == b'}' && bytes[j + 1] == b'}' {
                    found = Some(j);
                    break;
                }
                j += 1;
            }
            if let Some(comp_end) = found {
                // push preceding markdown
                if last < i {
                    let md = &input[last..i];
                    if !md.trim().is_empty() {
                        segments.push(Segment::Markdown(md.to_string()));
                    }
                }
                // parse component snippet
                let inner = input[comp_start..comp_end].trim();
                if !inner.is_empty() {
                    match inner.parse::<TokenStream2>() {
                        Ok(ts) => segments.push(Segment::Component(ts)),
                        Err(_) => {
                            // Gracefully fall back to rendering literally.
                            segments.push(Segment::Markdown(format!("{{{{{inner}}}}}")));
                        }
                    }
                }
                i = comp_end + 2; // skip closing "}}"
                last = i;
                continue;
            } else {
                // No closing delimiter; treat remainder as Markdown.
                break;
            }
        }

        i += 1;
    }

    // Tail
    if last < input.len() {
        let tail = &input[last..];
        if !tail.trim().is_empty() {
            segments.push(Segment::Markdown(tail.to_string()));
        }
    }

    if segments.is_empty() {
        segments.push(Segment::Markdown(input.to_string()));
    }

    segments
}

enum MacroInput {
    Literal(LitStr),
    File { lit: LitStr },
    Url { lit: LitStr },
}

impl Parse for MacroInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Lit) {
            let lit: Lit = input.parse()?;
            match lit {
                Lit::Str(s) => Ok(MacroInput::Literal(s)),
                _ => {
                    Err(input
                        .error("markdown_view!: expected a string literal or `file = \"...\"`"))
                }
            }
        } else {
            let ident: Ident = input.parse()?;
            match ident.to_string().as_str() {
                "file" | "path" => {
                    input.parse::<Token![=]>()?;
                    let lit: LitStr = input.parse()?;
                    Ok(MacroInput::File { lit })
                }
                "url" => {
                    input.parse::<Token![=]>()?;
                    let lit: LitStr = input.parse()?;
                    Ok(MacroInput::Url { lit })
                }
                _ => Err(input.error("markdown_view!: left side must be `file`, `path`, or `url`")),
            }
        }
    }
}

/// A Leptos-friendly procedural macro that converts Markdown into a `view!` tree
/// and allows embedding Leptos components inline, MDX-style.
///
/// Markdown is parsed with the following extensions enabled: tables, footnotes,
/// strikethrough, and task lists. The generated HTML is injected into a
/// `<div>` using the `inner_html` attribute inside a Leptos `view!` block.
///
/// To embed Leptos components within the Markdown, wrap them in `{{ ... }}`
/// inside the string literal. For example: `{{ <MyComponent/> }}`.
///
/// Examples
///
/// Basic usage with a string or raw string:
///
/// ```ignore
/// use markdown_view_leptos::markdown_view;
///
/// // Using a normal string
/// let _view = markdown_view!("# Title\n\nSome text.");
///
/// // Or a raw string to avoid escaping
/// let _view = markdown_view!(r#"# Title
///
/// Some text."#);
/// ```
///
/// Embedding a Leptos component:
///
/// ```ignore
/// use leptos::*;
/// use markdown_view_leptos::markdown_view;
///
/// #[component]
/// fn Hello() -> impl IntoView {
///     view! { <span>"Hi"</span> }
/// }
///
/// let _view = markdown_view!(r#"Before {{ <Hello/> }} After"#);
/// ```
#[proc_macro]
pub fn markdown_view(input: TokenStream) -> TokenStream {
    let parsed = parse_macro_input!(input as MacroInput);

    // Keep lit for include_str! emission
    let mut file_path_lit: Option<LitStr> = None;
    let mut url_lit: Option<LitStr> = None;

    let markdown_source: String = match parsed {
        MacroInput::Literal(lit) => lit.value(),
        MacroInput::File { lit } => {
            file_path_lit = Some(lit.clone());
            let manifest_dir =
                std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| String::from("."));
            let mut full_path = PathBuf::from(manifest_dir);
            full_path.push(lit.value());
            match fs::read_to_string(&full_path) {
                Ok(content) => content,
                Err(err) => {
                    let msg = format!(
                        "markdown_view!: failed to read file at '{}': {}",
                        full_path.display(),
                        err
                    );
                    return syn::Error::new(proc_macro2::Span::call_site(), msg)
                        .to_compile_error()
                        .into();
                }
            }
        }
        MacroInput::Url { lit } => {
            url_lit = Some(lit.clone());
            let url = lit.value();
            // Avoid heavy network work or potential proc-macro server quirks in rust-analyzer.
            let is_rust_analyzer = std::env::var_os("RUST_ANALYZER_INTERNALS_DO_NOT_USE").is_some()
                || std::env::var_os("RA_TEST").is_some()
                || std::env::var_os("RUST_ANALYZER").is_some();
            if is_rust_analyzer {
                // Return a lightweight placeholder so editor tooling doesn't panic.
                let expanded = quote! {{
                    ::leptos::view! { <div>"(remote Markdown preview disabled in rust-analyzer)"</div> }
                }};
                return expanded.into();
            }
            let client = match reqwest::blocking::Client::builder()
                .user_agent(concat!(
                    env!("CARGO_PKG_NAME"),
                    "/",
                    env!("CARGO_PKG_VERSION"),
                    " (+",
                    env!("CARGO_PKG_REPOSITORY"),
                    ")"
                ))
                .build()
            {
                Ok(c) => c,
                Err(err) => {
                    let msg = format!("markdown_view!: failed to build HTTP client: {}", err);
                    return syn::Error::new(proc_macro2::Span::call_site(), msg)
                        .to_compile_error()
                        .into();
                }
            };
            match client.get(&url).send() {
                Ok(resp) => match resp.error_for_status() {
                    Ok(ok) => match ok.text() {
                        Ok(body) => body,
                        Err(err) => {
                            let msg = format!(
                                "markdown_view!: failed reading response body from '{}': {}",
                                url, err
                            );
                            return syn::Error::new(proc_macro2::Span::call_site(), msg)
                                .to_compile_error()
                                .into();
                        }
                    },
                    Err(err) => {
                        let msg =
                            format!("markdown_view!: HTTP GET '{}' returned error: {}", url, err);
                        return syn::Error::new(proc_macro2::Span::call_site(), msg)
                            .to_compile_error()
                            .into();
                    }
                },
                Err(err) => {
                    let msg = format!("markdown_view!: HTTP GET '{}' failed: {}", url, err);
                    return syn::Error::new(proc_macro2::Span::call_site(), msg)
                        .to_compile_error()
                        .into();
                }
            }
        }
    };

    let segments = split_markdown_with_components(&markdown_source);

    let parts = segments.into_iter().map(|seg| match seg {
        Segment::Markdown(md) => {
            let html_output = convert_markdown_to_html(&md);
            quote! { <div inner_html={#html_output}></div> }
        }
        Segment::Component(ts) => {
            quote! { #ts }
        }
    });

    let include_stmt = if let Some(lit) = file_path_lit {
        // Turn file into an input dependency of the build to trigger recompiles.
        quote! { let _ = include_str!(::core::concat!(env!("CARGO_MANIFEST_DIR"), "/", #lit)); }
    } else if let Some(lit) = url_lit {
        // Create a pseudo-dependency so rebuilds can be triggered manually if desired.
        // For URLs, there's no stable file to watch, so we only embed the literal (no include_str!).
        let _url: LitStr = lit;
        quote! {}
    } else {
        quote! {}
    };

    let expanded = quote! {{
        #include_stmt
        ::leptos::view! {
            <div>
                #(#parts)*
            </div>
        }
    }};

    expanded.into()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn convert_markdown_to_html_basic() {
        let html = convert_markdown_to_html("# Title\n\nSome **bold** text.");
        assert!(html.contains("<h1>Title</h1>"));
        assert!(html.contains("<strong>bold</strong>"));
        assert!(html.contains("<p>"));
    }

    #[test]
    fn convert_markdown_to_html_empty() {
        let html = convert_markdown_to_html("");
        assert_eq!(html, "");
        let html_ws = convert_markdown_to_html("   \n\t\n");
        assert_eq!(html_ws, "");
    }

    #[test]
    fn split_markdown_with_components_simple() {
        let input = "Hello {{ <MyComp/> }} world";
        let segments = split_markdown_with_components(input);
        assert_eq!(segments.len(), 3);
        match &segments[0] {
            Segment::Markdown(s) => assert_eq!(s, "Hello "),
            _ => panic!("expected markdown"),
        }
        match &segments[1] {
            Segment::Component(_) => {}
            _ => panic!("expected component"),
        }
        match &segments[2] {
            Segment::Markdown(s) => assert_eq!(s, " world"),
            _ => panic!("expected markdown"),
        }
    }

    #[test]
    fn split_markdown_with_components_unclosed() {
        let input = "Hello {{";
        let segments = split_markdown_with_components(input);
        assert_eq!(segments.len(), 1);
        match &segments[0] {
            Segment::Markdown(s) => assert_eq!(s, "Hello {{"),
            _ => panic!("expected markdown"),
        }
    }

    #[test]
    fn split_markdown_with_components_only_text() {
        let input = "Just text";
        let segments = split_markdown_with_components(input);
        assert_eq!(segments.len(), 1);
        match &segments[0] {
            Segment::Markdown(s) => assert_eq!(s, "Just text"),
            _ => panic!("expected markdown"),
        }
    }

    #[test]
    fn split_ignores_embeds_inside_code_fences() {
        let input =
            "````\ncode with {{ <Nope/> }} inside\n````\nAfter {{ <Yes/> }}".replace("````", "```");
        let segments = split_markdown_with_components(&input);
        // Expect two segments: code fence (markdown), then a component and trailing markdown.
        assert!(segments.len() >= 2);
        match &segments[0] {
            Segment::Markdown(s) => assert!(s.contains("code with {{ <Nope/> }} inside")),
            _ => panic!("expected markdown for code fence block"),
        }
        assert!(segments.iter().any(|s| matches!(s, Segment::Component(_))));
    }

    #[test]
    fn split_handles_multiline_component() {
        let input = "Before\n\n{{\n    <MyComp\n        foo=123\n        bar=\"baz\"\n    />\n}}\n\nAfter";
        let segments = split_markdown_with_components(input);
        assert_eq!(segments.len(), 3);
        assert!(matches!(segments[0], Segment::Markdown(_)));
        assert!(matches!(segments[1], Segment::Component(_)));
        assert!(matches!(segments[2], Segment::Markdown(_)));
    }

    #[test]
    fn split_multiple_components() {
        let input = "A {{ <One/> }} B {{ <Two/> }} C";
        let segments = split_markdown_with_components(input);
        assert_eq!(segments.len(), 5);
        assert!(matches!(segments[0], Segment::Markdown(_)));
        assert!(matches!(segments[1], Segment::Component(_)));
        assert!(matches!(segments[2], Segment::Markdown(_)));
        assert!(matches!(segments[3], Segment::Component(_)));
        assert!(matches!(segments[4], Segment::Markdown(_)));
    }
}

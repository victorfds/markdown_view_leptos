//! markdown_view_leptos — Render Markdown at compile time with inline Leptos components
//!
//! This crate provides a single procedural macro:
//!
//! - `markdown_view!`: Converts a string literal or `file = "..."`/`url = "..."` into
//!   a Leptos `view!` tree at compile time; dynamic strings render at runtime.
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
    parse::discouraged::Speculative, parse::Parse, parse::ParseStream, parse_macro_input, Expr,
    ExprLit, Ident, Lit, LitBool, LitStr, Token,
};

fn strip_front_matter(input: &str) -> &str {
    if !(input.starts_with("---\n") || input.starts_with("---\r\n")) {
        return input;
    }
    let bytes = input.as_bytes();
    let mut i = if input.starts_with("---\r\n") { 5 } else { 4 }; // position after opening line
    while i + 3 <= bytes.len() {
        if bytes[i..].starts_with(b"---") {
            // Require preceding newline to avoid matching inline occurrences.
            if i > 0 && bytes[i - 1] == b'\n' {
                let after = i + 3;
                if after <= bytes.len() {
                    if bytes.get(after) == Some(&b'\r') && bytes.get(after + 1) == Some(&b'\n') {
                        return &input[after + 2..];
                    }
                    if bytes.get(after) == Some(&b'\n') {
                        return &input[after + 1..];
                    }
                    // End of string right after closing fence.
                    if after == bytes.len() {
                        return "";
                    }
                }
            }
        }
        i += 1;
    }
    input
}

fn convert_markdown_to_html(markdown: impl AsRef<str>, should_strip_front_matter: bool) -> String {
    let markdown = if should_strip_front_matter {
        strip_front_matter(markdown.as_ref())
    } else {
        markdown.as_ref()
    };
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
fn split_markdown_with_components(input: impl AsRef<str>) -> Vec<Segment> {
    let input = input.as_ref();
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

#[derive(Debug)]
enum Source {
    Inline(LitStr),
    File(LitStr),
    Url(LitStr),
    /// Any other expression; rendered at runtime without inline component expansion.
    Dynamic(Expr),
}

impl Parse for Source {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Ident) && input.peek2(Token![=]) {
            let ident: Ident = input.parse()?;
            input.parse::<Token![=]>()?;
            let lit: LitStr = input.parse()?;
            return match ident.to_string().as_str() {
                "file" | "path" => Ok(Source::File(lit)),
                "url" => Ok(Source::Url(lit)),
                _ => {
                    Err(input.error("markdown_view!: expected `file`, `path`, or `url` before `=`"))
                }
            };
        }

        let expr: Expr = input.parse()?;
        Ok(match resolve_expr_to_lit(&expr) {
            Some(lit) => Source::Inline(lit),
            None => Source::Dynamic(expr),
        })
    }
}

#[derive(Debug)]
struct MacroArgs {
    strip_front_matter: bool,
    source: Source,
}

fn extract_str_literal(expr: &Expr) -> Option<LitStr> {
    match expr {
        Expr::Lit(ExprLit {
            lit: Lit::Str(s), ..
        }) => Some(s.clone()),
        Expr::Call(call) => {
            if call.args.len() != 1 {
                return None;
            }
            // Allow String::from("...") and std::string::String::from("...").
            let is_string_from = match call.func.as_ref() {
                Expr::Path(path) => {
                    let mut segs = path.path.segments.iter().rev();
                    matches!(
                        (segs.next(), segs.next()),
                        (Some(last), Some(prev)) if last.ident == "from" && prev.ident == "String"
                    )
                }
                _ => false,
            };
            if !is_string_from {
                return None;
            }
            match call.args.first() {
                Some(Expr::Lit(ExprLit {
                    lit: Lit::Str(s), ..
                })) => Some(s.clone()),
                _ => None,
            }
        }
        Expr::MethodCall(method) => {
            let name = method.method.to_string();
            if (name != "to_string" && name != "to_owned") || !method.args.is_empty() {
                return None;
            }
            match method.receiver.as_ref() {
                Expr::Lit(ExprLit {
                    lit: Lit::Str(s), ..
                }) => Some(s.clone()),
                _ => None,
            }
        }
        _ => None,
    }
}
fn resolve_expr_to_lit(expr: &Expr) -> Option<LitStr> {
    extract_str_literal(expr)
}

impl Parse for MacroArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut strip_front_matter = false;

        // Optional `strip_front_matter = true/false,` prefix.
        if input.peek(Ident) && input.peek2(Token![=]) {
            let fork = input.fork();
            let ident: Ident = fork.parse()?;
            if ident == "strip_front_matter" {
                fork.parse::<Token![=]>()?;
                let flag: LitBool = fork.parse()?;
                strip_front_matter = flag.value();
                if fork.peek(Token![,]) {
                    fork.parse::<Token![,]>()?;
                }
                input.advance_to(&fork);
            }
        }

        if input.is_empty() {
            return Err(input.error("markdown_view!: expected a source argument"));
        }

        let source = if input.peek(Ident) && input.peek2(Token![=]) {
            let ident: Ident = input.parse()?;
            input.parse::<Token![=]>()?;
            let lit: LitStr = input.parse()?;
            match ident.to_string().as_str() {
                "file" | "path" => Source::File(lit),
                "url" => Source::Url(lit),
                _ => {
                    return Err(
                        input.error("markdown_view!: expected `file`, `path`, or `url` before `=`")
                    )
                }
            }
        } else {
            let expr: Expr = input.parse()?;
            match resolve_expr_to_lit(&expr) {
                Some(lit) => Source::Inline(lit),
                None => Source::Dynamic(expr),
            }
        };

        if input.peek(Token![,]) {
            let _ = input.parse::<Token![,]>();
        }
        if !input.is_empty() {
            return Err(input.error("markdown_view!: unexpected tokens after source"));
        }

        Ok(MacroArgs {
            strip_front_matter,
            source,
        })
    }
}

/// A Leptos-friendly procedural macro that converts Markdown into a `view!` tree
/// and allows embedding Leptos components inline, MDX-style.
///
/// Markdown is parsed with the following extensions enabled: tables, footnotes,
/// strikethrough, and task lists. The generated HTML is injected into a
/// `<div>` using the `inner_html` attribute inside a Leptos `view!` block.
///
/// To embed Leptos components within the Markdown, wrap them in `{{ ... }}` inside
/// the string literal. For example: `{{ <MyComponent/> }}`.
/// Inline component expansion happens whenever the macro can resolve the string
/// content at compile time (string literal or `file`/`url`). If the value is
/// only known at runtime, the Markdown is rendered without expanding `{{ ... }}`.
///
/// Options:
/// - Prefix the invocation with `strip_front_matter = true,` to remove a leading
///   YAML front matter block (`--- ... ---`) before rendering. The flag works
///   with inline strings as well as `file`/`url` sources.
///
/// Examples
///
/// Using an owned `String` wrapper:
///
/// ```ignore
/// use markdown_view_leptos::markdown_view;
///
/// let view = markdown_view!(String::from("# Title from String"));
/// ```
///
/// Basic usage with a string or raw string:
///
/// ```ignore
/// use markdown_view_leptos::markdown_view;
///
/// // Using a normal string
/// let view = markdown_view!("# Title\n\nSome text.");
///
/// // Or a raw string to avoid escaping
/// let view = markdown_view!(r#"# Title
///
/// Some text."#);
/// ```
///
/// Embedding a Leptos component:
///
/// ```ignore
/// use leptos::prelude::*;
/// use markdown_view_leptos::markdown_view;
///
/// #[component]
/// fn Hello() -> impl IntoView {
///     view! { <span>"Hi"</span> }
/// }
///
/// let view = markdown_view!(r#"Before {{ <Hello/> }} After"#);
/// ```
///
/// Stripping YAML front matter (options come before the source):
///
/// ```ignore
/// use markdown_view_leptos::markdown_view;
///
/// let view = markdown_view!(
///     strip_front_matter = true,
///     r#"---
/// title: Example
/// ---
///
/// # Hello
/// Body text.
/// "#
/// );
/// ```
///
/// The same flag can be combined with `file` or `url` sources:
///
/// ```ignore
/// let view = markdown_view!(
///     strip_front_matter = true,
///     file = "content/post.md"
/// );
/// ```
///
/// Dynamic Markdown at runtime (inline components inside `{{ ... }}` are not expanded):
///
/// ```ignore
/// use markdown_view_leptos::markdown_view;
///
/// let markdown_body: String = load_markdown_somehow();
/// let view = markdown_view!(markdown_body);
/// ```
#[proc_macro]
pub fn markdown_view(input: TokenStream) -> TokenStream {
    let MacroArgs {
        strip_front_matter,
        source: parsed,
    } = parse_macro_input!(input as MacroArgs);

    // Keep lit for include_str! emission
    let mut file_path_lit: Option<LitStr> = None;
    let mut url_lit: Option<LitStr> = None;

    let markdown_source: String = match parsed {
        Source::Dynamic(expr) => {
            let expanded = quote! {{
                fn strip_front_matter(input: &str) -> &str {
                    if !(input.starts_with("---\n") || input.starts_with("---\r\n")) {
                        return input;
                    }
                    let bytes = input.as_bytes();
                    let mut i = if input.starts_with("---\r\n") { 5 } else { 4 };
                    while i + 3 <= bytes.len() {
                        if bytes[i..].starts_with(b"---") && i > 0 && bytes[i - 1] == b'\n' {
                            let after = i + 3;
                            if after <= bytes.len() {
                                if bytes.get(after) == Some(&b'\r') && bytes.get(after + 1) == Some(&b'\n') {
                                    return &input[after + 2..];
                                }
                                if bytes.get(after) == Some(&b'\n') {
                                    return &input[after + 1..];
                                }
                                if after == bytes.len() {
                                    return "";
                                }
                            }
                        }
                        i += 1;
                    }
                    input
                }
                fn render_markdown_to_html(markdown: impl AsRef<str>, strip_front_matter: bool) -> String {
                    let markdown = if strip_front_matter {
                        strip_front_matter(markdown.as_ref())
                    } else {
                        markdown.as_ref()
                    };
                    if markdown.trim().is_empty() {
                        return String::new();
                    }
                    let mut options = ::pulldown_cmark::Options::empty();
                    options.insert(::pulldown_cmark::Options::ENABLE_TABLES);
                    options.insert(::pulldown_cmark::Options::ENABLE_FOOTNOTES);
                    options.insert(::pulldown_cmark::Options::ENABLE_STRIKETHROUGH);
                    options.insert(::pulldown_cmark::Options::ENABLE_TASKLISTS);
                    let parser = ::pulldown_cmark::Parser::new_ext(markdown, options);
                    let mut html_output = String::new();
                    ::pulldown_cmark::html::push_html(&mut html_output, parser);
                    html_output
                }
                let __md_source = #expr;
                let __html = render_markdown_to_html(__md_source, #strip_front_matter);
                ::leptos::view! { <div inner_html={__html}></div> }
            }};
            return expanded.into();
        }
        Source::Inline(lit) => lit.value(),
        Source::File(lit) => {
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
        Source::Url(lit) => {
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
            let html_output = convert_markdown_to_html(&md, strip_front_matter);
            quote! { <div inner_html={#html_output}></div> }
        }
        Segment::Component(ts) => quote! { #ts },
    });

    let include_stmt = if let Some(lit) = file_path_lit {
        // Turn file into an input dependency of the build to trigger recompiles.
        quote! { let _ = include_str!(::core::concat!(env!("CARGO_MANIFEST_DIR"), "/", #lit)); }
    } else if let Some(lit) = url_lit {
        let _ = lit; // keep future-proofed: url literal still available for diagnostics
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
        let html = convert_markdown_to_html("# Title\n\nSome **bold** text.", false);
        assert!(html.contains("<h1>Title</h1>"));
        assert!(html.contains("<strong>bold</strong>"));
        assert!(html.contains("<p>"));
    }

    #[test]
    fn convert_markdown_to_html_accepts_string() {
        let html = convert_markdown_to_html(String::from("just **bold** text"), false);
        assert!(html.contains("<strong>bold</strong>"));
    }

    #[test]
    fn convert_markdown_to_html_empty() {
        let html = convert_markdown_to_html("", false);
        assert_eq!(html, "");
        let html_ws = convert_markdown_to_html("   \n\t\n", false);
        assert_eq!(html_ws, "");
    }

    #[test]
    fn convert_markdown_strips_front_matter() {
        let html =
            convert_markdown_to_html("---\ntitle: Example\n---\n\nContent with **bold**", true);
        assert!(html.contains("Content with"));
        assert!(!html.contains("title: Example"));
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
    fn split_markdown_with_components_string_input() {
        let input = String::from("Owned {{ <MyComp/> }} value");
        let segments = split_markdown_with_components(input);
        assert_eq!(segments.len(), 3);
        assert!(matches!(segments[0], Segment::Markdown(_)));
        assert!(matches!(segments[1], Segment::Component(_)));
        assert!(matches!(segments[2], Segment::Markdown(_)));
    }

    #[test]
    fn macro_input_accepts_string_from_literal() {
        let parsed: Source = syn::parse_str(r#"String::from("Owned")"#).unwrap();
        match parsed {
            Source::Inline(lit) => assert_eq!(lit.value(), "Owned"),
            _ => panic!("expected literal variant"),
        }
    }

    #[test]
    fn macro_input_allows_dynamic_expression() {
        let parsed: Source = syn::parse_str("some_variable").unwrap();
        assert!(matches!(parsed, Source::Dynamic(_)));
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
        let input =
            "Before\n\n{{\n    <MyComp\n        foo=123\n        bar=\"baz\"\n    />\n}}\n\nAfter";
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

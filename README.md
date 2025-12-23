# markdown_view_leptos

Compile-time Markdown to Leptos `view!` with MDX‑like inline components.

- Inline components in Markdown using `{{ <MyComp prop=value/> }}`
- Use string, `file = "..."`, or `url = "..."` sources
- Fenced‑code aware: ignores `{{ ... }}` inside triple‑backtick code blocks
- Works in CSR or SSR; compile-time sources avoid runtime parser cost in the browser

> Security note: the generated HTML is injected via `inner_html`. Only use trusted Markdown or sanitize upstream.

## Install

```
cargo add markdown_view_leptos
```

For CSR (WASM):
- `rustup target add wasm32-unknown-unknown`
- Use a bundler like Trunk (`cargo install trunk`)

## Quick start

```rust
use leptos::prelude::*;
use markdown_view_leptos::markdown_view;

#[component]
fn Hello() -> impl IntoView {
    view! { <em>"Hello from a component"</em> }
}

#[component]
pub fn App() -> impl IntoView {
    view! { <main>{markdown_view!(r#"
# Title

Some text before the component.

{{ <Hello/> }}

And some text after.
"#)}</main> }
}
```

## Macro inputs

Both `markdown_view!` and `markdown_anchors!` accept the same input forms:

- Inline string literals (`"..."`, `r#"..."#`, `String::from("...")`, `"...".to_string()`).
- `file = "path.md"`: resolved at compile time relative to `CARGO_MANIFEST_DIR`.
- `file = <expr>`: resolved at compile time if the file exists; otherwise read at runtime
  (non-wasm only).
- `url = "https://..."`: fetched at compile time (disabled in rust-analyzer). If the URL
  expression cannot be resolved to a literal, the macro treats it like a dynamic
  Markdown string expression.
- Any other expression (`String`/`&str`): handled at runtime.

### Inline string
Works with normal or raw strings; inline `{{ ... }}` components are spliced in.
The same inline forms work with `markdown_anchors!`.

```rust
use leptos::prelude::*;
use markdown_view_leptos::markdown_view;

#[component]
pub fn App() -> impl IntoView {
    view! { <section>{markdown_view!(file = "content.md")}</section> }
}
```

### From a file
`file` paths are resolved relative to your crate root (`CARGO_MANIFEST_DIR`). Literal paths are embedded at compile time so edits trigger recompiles.

You can also point to a variable. If the macro can resolve it at compile time (e.g., `let content = "content.md";` or `format!("content.md")` where the file exists), it behaves like a literal path. Otherwise it falls back to reading at runtime (non-wasm only) and inline components are not expanded.

`markdown_anchors!` accepts the same `file = ...` forms.

```rust
let base = "/opt/articles";
let name = "welcome.md";
let view = markdown_view!(file = format!("{}/{}", base, name));
```

For wasm builds, use a path the macro can resolve at compile time so the content is embedded (no filesystem at runtime).

### Dynamic string at runtime
`String`/`&str` variables are accepted.

```rust
let body = r#"
# Title

Inline component: {{ <Hello/> }}
"#;
let inline_view = markdown_view!(body); // components work: `body` is a literal binding

let runtime_body: String = fetch_from_server();
let runtime_view = markdown_view!(runtime_body); // runtime parsing, `{{ ... }}` stays literal
```

If the macro can see a string literal binding in the same file (as with `body` above),
it inlines it so `{{ ... }}` components still render. Otherwise Markdown is rendered
at runtime and `{{ ... }}` blocks stay literal text.

`markdown_anchors!` uses the same runtime path for dynamic strings.

### From a URL (build-time fetch)

```rust
use leptos::prelude::*;
use markdown_view_leptos::markdown_view;
#[component] 
pub fn App() -> impl IntoView {
    view! { 
            <div>{markdown_view!(url = "https://github.com/leptos-rs/awesome-leptos/blob/main/README.md")}</div>
          }
}
```

- Happens at compile‑time (not client runtime), using a blocking HTTP GET.
- For editor tooling (rust‑analyzer), remote fetch is disabled and a small placeholder is returned for responsiveness.
- Remote fetch happens at build time on non‑wasm targets (blocking HTTP GET). rust‑analyzer and wasm builds get a placeholder for responsiveness. Network errors fall back to a placeholder; otherwise the fetched content is embedded.
- Prefer `file = "..."` for reproducible builds.

`markdown_anchors!` accepts the same `url = ...` forms.

## How it works

- Markdown → HTML: Compile-time sources use `pulldown‑cmark` with the full option set (definition lists, footnotes, GFM, math, heading attributes, metadata blocks, and more). Runtime strings use a lightweight built-in parser (headings + paragraphs + heading IDs). Injected via `inner_html` into a `view!` tree.
- Inline components: Any `{{ ... }}` outside fenced code blocks is parsed as Rust/RSX and spliced into the `view!` tree for compile-time sources (string literal, `file`, `url`, or identifiers that resolve to literals in the same file). Runtime `String` inputs render `{{ ... }}` literally.
- Fenced code: Triple‑backtick fences (```) are respected; `{{ ... }}` inside them is ignored and rendered literally.
- Parse fallback: If a snippet inside `{{ ... }}` doesn’t parse, it is rendered as plain Markdown so your build doesn’t fail unexpectedly.
- Metadata blocks: Compile-time sources honor YAML/TOML front matter (`---` or `+++`) via pulldown‑cmark’s metadata block options.
- Anchor extraction: `markdown_anchors!` uses pulldown‑cmark at compile time; runtime strings use the lightweight parser and honor `{#id}` (other attributes are ignored).

## Options: heading anchors

Headings get IDs plus an anchor link (rendered before the heading text). IDs are slugified by lowercasing, stripping accents, and replacing non-alphanumeric runs with `-` (for example, `Último parágrafo` becomes `ultimo-paragrafo`). Customize or disable:

```rust
let view = markdown_view!(
    file = "content.md",
    anchor = true,
    anchor_class = "my-anchor",
    anchor_style = "color: #f40;",
    anchor_wrapper_class = "anchor-wrap",
    anchor_wrapper_style = "display: inline-block;",
    anchor_symbol = "§"
);
```

`anchor_wrapper_class` and `anchor_wrapper_style` apply to the heading element
that wraps the anchor link for additional layout control.

To disable anchor links (IDs still render for deep links):

```rust
let view = markdown_view!(
    file = "content.md",
    anchor = false
);
```

Anchor styling is handled via CSS. A VitePress-like pattern:

```css
.markdown-view { --mdv-anchor-color: #6b7280; --mdv-anchor-hover-color: #111827; }
.markdown-view :is(h1, h2, h3, h4, h5, h6) { position: relative; }
.markdown-view .header-anchor {
  position: absolute;
  left: -0.9em;
  opacity: 0;
  text-decoration: none;
  color: var(--mdv-anchor-color);
  transition: opacity 0.15s ease, color 0.15s ease;
}
.markdown-view :is(h1, h2, h3, h4, h5, h6):hover .header-anchor {
  opacity: 1;
  color: var(--mdv-anchor-hover-color);
}
```

## Utility: collect anchors

Use `markdown_anchors!` to get all heading `(title, id)` pairs (for TOCs or navigation). The same slug rules apply when headings do not define a custom `{#id}`:

```rust
use markdown_view_leptos::markdown_anchors;

let anchors = markdown_anchors!(r#"
# Getting Started
## Install
## Install {#custom-install}
"#);
// anchors == [
//   ("Getting Started", "getting-started"),
//   ("Install", "install"),
//   ("Install", "custom-install")
// ]

let toc = anchors
    .iter()
    .map(|(title, id)| format!("<li><a href=\"#{}\">{}</a></li>", id, title))
    .collect::<String>();
```

`markdown_anchors!` only returns anchors when the input contains `[[toc]]`. The marker
is stripped from `markdown_view!` output and never rendered.

When you pass a dynamic string expression to `markdown_anchors!`, the runtime path
uses the lightweight parser and honors `{#id}` (other heading attributes are ignored).

## Example

This repo includes a CSR example (Trunk):

- `examples/markdown-csr-example`: Renders a canvas‑based `ParticleText` component from within Markdown.

Run with Trunk:

```
cd examples/markdown-csr-example
trunk serve --open
```

## Tips & caveats

- Scope: Components referenced inside `{{ ... }}` must be in scope where you invoke the macro.
- Styling: Wrap the macro output with your own container and styles, then target markdown elements with CSS.
- Sanitization: If you need strict XSS safety, sanitize before compile time or filter the source.
- Rebuilds: `file = "..."` uses `include_str!` to make the file a build input; saving it triggers a rebuild.

## Testing & contributing

- Run tests: `cargo test`
- Lint & format: `cargo clippy --all-targets -- -D warnings` and `cargo fmt --all`
- Issues & PRs welcome. Keep scopes small and messages clear (Conventional Commits).

---

Thank you for reading this 💙

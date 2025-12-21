# markdown_view_leptos

Compile-time Markdown to Leptos `view!` with MDX‑like inline components.

- Inline components in Markdown using `{{ <MyComp prop=value/> }}`
- Use string, `file = "..."`, or `url = "..."` sources
- Fenced‑code aware: ignores `{{ ... }}` inside triple‑backtick code blocks
- Works in CSR or SSR; no runtime parser cost in the browser

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

### Inline string
Works with normal or raw strings; inline `{{ ... }}` components are spliced in.

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

You can also point to a variable. If the macro can resolve it at compile time (e.g., `let content = "content.md";` or `format!("content.md")` where the file exists), it behaves like a literal path. Otherwise it falls back to reading at runtime (non-wasm only) and inline components are still resolved when the view renders.

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
let runtime_view = markdown_view!(runtime_body); // runtime parsing, components expand at runtime
```

If the macro can see a string literal binding in the same file (as with `body` above),
it inlines it so `{{ ... }}` components still render. Otherwise Markdown is rendered
at runtime and `{{ ... }}` blocks are expanded there as well, so components always work.

## From a URL (build‑time fetch)

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

## How it works

- Markdown → HTML: Compile-time sources use `pulldown‑cmark` (tables, footnotes, strikethrough, task lists); runtime strings are parsed with a built-in lightweight renderer so no extra dependencies are needed. Injected via `inner_html` into a `view!` tree.
- Inline components: Any `{{ ... }}` outside fenced code blocks is parsed as Rust/RSX and spliced into the `view!` tree (for compile-time sources: string literal, `file`, `url`, or identifiers that resolve to literals in the same file). Runtime `String` inputs go through the lightweight renderer, which now also expands `{{ ... }}` so components work regardless of when the string is known.
- Fenced code: Triple‑backtick fences (```) are respected; `{{ ... }}` inside them is ignored and rendered literally.
- Parse fallback: If a snippet inside `{{ ... }}` doesn’t parse, it is rendered as plain Markdown so your build doesn’t fail unexpectedly.
- Front matter: Pass `strip_front_matter = true` to drop a leading `--- ... ---` block (YAML-style) before rendering if you don't want it to show up.

## Options: strip front matter

When your Markdown carries YAML front matter, prefix the macro with `strip_front_matter = true` before the source.

```rust
use leptos::prelude::*;
use markdown_view_leptos::markdown_view;

#[component]
pub fn Article() -> impl IntoView {
    view! {
        <article>
            {markdown_view!(
                strip_front_matter = true,
                file = "content/hello-world.md"
            )}
        </article>
    }
}
```

The flag also works with inline strings:

```rust
let view = markdown_view!(
    strip_front_matter = true,
    r#"---
title: Hello World
---

# Hello World

Body text goes here.
"#
);
```

## Options: heading anchors

Headings get IDs plus an anchor link (rendered before the heading text). IDs are slugified by lowercasing, stripping accents, and replacing non-alphanumeric runs with `-` (for example, `Último parágrafo` becomes `ultimo-paragrafo`). Customize or disable:

```rust
let view = markdown_view!(
    file = "content.md",
    anchor = true,
    anchor_class = "my-anchor",
    anchor_style = "color: #f40;",
    anchor_symbol = "§"
);
```

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

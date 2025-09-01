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
    // Embed a real component inside Markdown using {{ <Hello/> }}
    let md = r#"
# Title

Some text before the component.

{{ <Hello/> }}

And some text after.
"#;

    view! { <main>{markdown_view!(md)}</main> }
}
```

## From a File
`file` paths are resolved relative to your crate root (`CARGO_MANIFEST_DIR`).

Below is a live Leptos component:

```rust
use leptos::prelude::*;
use markdown_view_leptos::markdown_view;

#[component]
pub fn App() -> impl IntoView {
    view! { <section>{markdown_view!(file = "content.md")}</section> }
}
```

When using `file = "..."`, the macro emits an `include_str!` so edits to the file trigger recompiles.

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
- Prefer `file = "..."` for reproducible builds.

## How it works

- Markdown → HTML: Parsed with `pulldown‑cmark` (tables, footnotes, strikethrough, task lists). Injected via `inner_html` into a `view!` tree.
- Inline components: Any `{{ ... }}` outside fenced code blocks is parsed as Rust/RSX and spliced into the `view!` tree. The snippet must be valid in scope (e.g., `<MyComp/>`).
- Fenced code: Triple‑backtick fences (```) are respected; `{{ ... }}` inside them is ignored and rendered literally.
- Parse fallback: If a snippet inside `{{ ... }}` doesn’t parse, it is rendered as plain Markdown so your build doesn’t fail unexpectedly.

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

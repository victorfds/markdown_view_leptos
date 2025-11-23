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

### Dynamic `String` input
You can also pass a `String`/`&str` variable to the macro. Dynamic inputs render Markdown at runtime without expanding inline components inside `{{ ... }}`.

```rust
let markdown_body = load_markdown_from_disk();
let view = markdown_view!(markdown_body);
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
- Inline components: Any `{{ ... }}` outside fenced code blocks is parsed as Rust/RSX and spliced into the `view!` tree (for compile-time sources: string literal, `file`, or `url`). Dynamic `String` inputs render as plain Markdown without expanding `{{ ... }}`.
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

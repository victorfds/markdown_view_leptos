# markdown-csr-example

Client-side Leptos app that renders Markdown via `markdown_view!` and embeds a live `ParticleText` component with `{{ <ParticleText/> }}`.

## Prerequisites

- Rust nightly channel
- WASM target: `rustup target add wasm32-unknown-unknown`
- Trunk: `cargo install trunk`

## Run (dev)

```
cd examples/markdown-csr-example
trunk serve --open
```

- Trunk builds the WASM and serves `index.html`.
- The app reads `content.md` at compile time, so edits trigger a rebuild.

## What’s inside

- `src/app.rs`: Defines `ParticleText` (canvas + physics) and `App` which renders `{ markdown_view!(file = "content.md") }`.
- `content.md`: Markdown content with `{{ <ParticleText/> }}` to mount the component.
- `index.html`: Minimal Trunk HTML with `<link data-trunk rel="rust" />`.

## Tips

- Adjust physics and sampling in `src/app.rs` for density/feel.
- Components referenced in Markdown must be in Rust scope where `markdown_view!` is used.


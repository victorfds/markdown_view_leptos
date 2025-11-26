use leptos::prelude::*;
use markdown_view_leptos::markdown_view;
use wasm_bindgen::{prelude::Closure, JsCast, JsValue};
use web_sys::{CanvasRenderingContext2d, HtmlCanvasElement};

#[derive(Clone)]
struct Particle {
    home_x: f64,
    home_y: f64,
    x: f64,
    y: f64,
    vx: f64,
    vy: f64,
}

#[component]
pub fn ParticleText() -> impl IntoView {
    // Particles and mouse state
    let particles: RwSignal<Vec<Particle>> = RwSignal::new(vec![]);
    let mouse: RwSignal<Option<(f64, f64)>> = RwSignal::new(None);

    // Mouse handlers (use offset_x/y for simplicity)
    let on_move = {
        move |ev: leptos::ev::MouseEvent| {
            mouse.set(Some((ev.offset_x() as f64, ev.offset_y() as f64)));
        }
    };
    let on_leave = { move |_ev: leptos::ev::MouseEvent| mouse.set(None) };

    Effect::new({
        let particles = particles.clone();
        let mouse = mouse.clone();
        move |_| {
            let document = web_sys::window().unwrap().document().unwrap();
            if let Some(el) = document.get_element_by_id("particle-text") {
                let canvas: HtmlCanvasElement = el.dyn_into::<HtmlCanvasElement>().unwrap();

                // Size and 2D context
                let width = 1200.0;
                let height = 600.0;
                canvas.set_width(width as u32);
                canvas.set_height(height as u32);

                let ctx = canvas
                    .get_context("2d")
                    .unwrap()
                    .unwrap()
                    .dyn_into::<CanvasRenderingContext2d>()
                    .unwrap();

                // Build initial particles from text
                let mut pts =
                    build_particles_from_text("Leptos is awesome!", width as u32, height as u32);
                if pts.is_empty() {
                    pts.push(Particle {
                        home_x: width / 2.0,
                        home_y: height / 2.0,
                        x: width / 2.0,
                        y: height / 2.0,
                        vx: 0.0,
                        vy: 0.0,
                    });
                }
                particles.set(pts);

                // Start animation
                start_animation_loop(canvas, ctx, particles, mouse);
            }
        }
    });

    view! {
        <canvas
            id="particle-text"
            on:mousemove=on_move
            on:mouseleave=on_leave
            style="display:block;max-width:100%;border:1px solid #1e293b;background:#0b1020"
        />
    }
}

fn build_particles_from_text(text: &str, w: u32, h: u32) -> Vec<Particle> {
    let document = web_sys::window().unwrap().document().unwrap();

    let offscreen: HtmlCanvasElement = document
        .create_element("canvas")
        .unwrap()
        .dyn_into::<HtmlCanvasElement>()
        .unwrap();
    offscreen.set_width(w);
    offscreen.set_height(h);
    let ctx = offscreen
        .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into::<CanvasRenderingContext2d>()
        .unwrap();

    set_fill_style(&ctx, "#ffffff");
    ctx.set_font("bold 120px system-ui, sans-serif");
    ctx.set_text_align("center");
    ctx.set_text_baseline("middle");
    ctx.clear_rect(0.0, 0.0, w as f64, h as f64);
    let _ = ctx.fill_text(text, w as f64 / 2.0, h as f64 / 2.0);

    let image = ctx
        .get_image_data(0.0, 0.0, w as f64, h as f64)
        .expect("get_image_data");
    let data = image.data(); // Clamped<Vec<u8>>

    let step = 4u32; // sampling stride to reduce particle count
    let mut particles = Vec::new();

    for y in (0..h).step_by(step as usize) {
        for x in (0..w).step_by(step as usize) {
            let idx = ((y * w + x) * 4) as usize;
            let a = data[idx + 3];
            if a > 128 {
                let jitter_x = (js_sys::Math::random() - 0.5) * 2.0;
                let jitter_y = (js_sys::Math::random() - 0.5) * 2.0;
                particles.push(Particle {
                    home_x: x as f64,
                    home_y: y as f64,
                    x: x as f64 + jitter_x,
                    y: y as f64 + jitter_y,
                    vx: 0.0,
                    vy: 0.0,
                });
            }
        }
    }
    particles
}

fn start_animation_loop(
    canvas: HtmlCanvasElement,
    ctx: CanvasRenderingContext2d,
    particles: RwSignal<Vec<Particle>>,
    mouse: RwSignal<Option<(f64, f64)>>,
) {
    use std::cell::RefCell;
    use std::rc::Rc;

    let friction = 0.86;
    let spring = 0.08;
    let repel_radius = 60.0;
    let repel_radius_sq = repel_radius * repel_radius;
    let repel_strength = 2200.0;

    // Self-referential RAF loop stored and leaked to keep alive.
    let raf_cell: Rc<RefCell<Option<Closure<dyn FnMut(f64)>>>> = Rc::new(RefCell::new(None));
    let raf_cell_clone = raf_cell.clone();

    let cb = Closure::wrap(Box::new(move |_ts: f64| {
        // Update physics
        particles.update(|pts| {
            if pts.is_empty() {
                return;
            }
            let mpos = mouse.get_untracked();
            for p in pts.iter_mut() {
                if let Some((mx, my)) = mpos {
                    let dx = p.x - mx;
                    let dy = p.y - my;
                    let d2 = dx * dx + dy * dy;
                    if d2 < repel_radius_sq && d2 > 0.5 {
                        let d = d2.sqrt();
                        let fx = (dx / d) * (repel_strength / (d2 + 1.0));
                        let fy = (dy / d) * (repel_strength / (d2 + 1.0));
                        p.vx += fx;
                        p.vy += fy;
                    }
                }
                // Spring toward home
                p.vx += (p.home_x - p.x) * spring;
                p.vy += (p.home_y - p.y) * spring;
                // Damping
                p.vx *= friction;
                p.vy *= friction;
                p.x += p.vx;
                p.y += p.vy;
            }
        });

        // Draw
        let w = canvas.width() as f64;
        let h = canvas.height() as f64;
        set_fill_style(&ctx, "#0b1020");
        ctx.fill_rect(0.0, 0.0, w, h);
        set_fill_style(&ctx, "#38bdf8");

        let pts = particles.get_untracked();
        for p in pts.iter() {
            ctx.fill_rect(p.x - 1.0, p.y - 1.0, 2.0, 2.0);
        }

        // Next frame
        web_sys::window()
            .unwrap()
            .request_animation_frame(
                raf_cell_clone
                    .borrow()
                    .as_ref()
                    .unwrap()
                    .as_ref()
                    .unchecked_ref(),
            )
            .unwrap();
    }) as Box<dyn FnMut(f64)>);

    // Store the closure so it can reference itself
    raf_cell.replace(Some(cb));

    // Kick off the loop
    web_sys::window()
        .unwrap()
        .request_animation_frame(raf_cell.borrow().as_ref().unwrap().as_ref().unchecked_ref())
        .unwrap();

    // Leak the cell to keep the RAF closure alive for the app lifetime.
    std::mem::forget(raf_cell);
}

#[component]
fn Counter() -> impl IntoView {
    let (count, set_count) = signal(0);

    view! {
        <button
            on:click=move |_| {
                set_count.update(|n| *n += 1);
            }
            style="padding:8px 16px;background:#38bdf8;color:#0b1020;border:none;border-radius:4px;cursor:pointer;font-weight:bold;font-size:14px"
        >

            "Click me: "
            {count}
        </button>
    }
}

#[component]
pub fn App() -> impl IntoView {
    let content = r"# Markdown CSR Example 
    This is a {{ <Counter /> }} inside markdown!
    Leptos is great
    
     - Markdown is great
     - CSR is great

     Enjoy using Leptos with Dynamic Markdown!";

    view! {
        <main style="font-family:system-ui, sans-serif; color:#e2e8f0; background:#0b1020; min-height:100vh; padding:24px;">
            {markdown_view!(content)} {markdown_view!(file = "content.md")}
        </main>
    }
}

fn set_fill_style(ctx: &CanvasRenderingContext2d, color: &str) {
    // Avoid deprecated set_fill_style by setting JS property directly
    let _ = js_sys::Reflect::set(
        ctx.as_ref(),
        &JsValue::from_str("fillStyle"),
        &JsValue::from_str(color),
    );
}

use leptos::prelude::*;
use leptos_meta::*;
use mihama::utils::RunningMode;
use mihama_core::transpiler::javascript::JavaScriptTranspiler;
use utils::{get_example_code, run_code};

use crate::example::PRELUDE_CODE;

mod example;
mod utils;

#[component]
pub fn App() -> impl IntoView {
    provide_meta_context();
    let (code, set_code) = signal(PRELUDE_CODE.to_string());
    let (js_code, set_js_code) = signal("".to_string());
    let (runtime, set_runtime) = signal("wasm".to_string());
    let (mode, set_mode) = signal("UNSAFE_EVALUATOR".to_string());
    let (view_type_info, set_view_type_info) = signal(true);
    let (output, set_output) = signal(String::new());
    let (error, set_error) = signal(String::new());

    let handle_run = move |_| {
        set_output.set("".to_string());
        set_error.set("".to_string());

        match run_code(
            code.get(),
            RunningMode::from(mode.get()),
            view_type_info.get(),
        ) {
            Ok(output) => set_output.set(output),
            Err(error) => set_error.set(error.to_string()),
        };
    };

    // let js_code_texturea = move || {
    //     if runtime.get() == "js" {
    //         view! { <div>"s"</div> }
    //     } else {
    //         view! { <div></div> }
    //     }
    // };

    view! {
        <Html attr:lang="en" attr:dir="ltr" attr:data-theme="light" />
        <Title text="Sena Playground" />
        <Meta charset="UTF-8" />
        <Meta name="viewport" content="width=device-width, initial-scale=1.0" />
        <div class="container">
            <h1>"Sena Playground"</h1>
            <r-divider></r-divider>
            <span>"Runtime:"</span>
            <select on:change:target=move |ev| {
                set_runtime.set(get_example_code(ev.target().value()).to_string());
            }>
                <option value="wasm">"WebAssembly"</option>
                <option value="js">"JavaScript"</option>
            </select>
            <br />
            <span>"Example Code:"</span>
            <select on:change:target=move |ev| {
                set_code.set(ev.target().value().to_string());
            }>
                <option value="">"Custom"</option>
                <option value="hello_world">"Hello, world!"</option>
                <option value="fibonaci">"Fibonacci series"</option>
                <option value="match">"Match expression"</option>
                <option value="list">"Define list"</option>
                <option value="infix">"Custom infix operators"</option>
                <option value="demo">"Demo code"</option>
                <option value="prelude">"Prelude"</option>
            </select>
            <br />
            <textarea
                on:input:target=move |ev| {
                    set_code.set(ev.target().value());
                }
                placeholder="Enter your code here..."
                prop:value=code
            />
            <r-button on:click=handle_run prop:type="info">"Run it!"</r-button>
            <input type="checkbox"
                on:change:target=move |ev| {
                    set_view_type_info.set(ev.target().checked());
                }
                prop:checked=view_type_info
            />
            <span>"View type information"</span>
            <select on:change:target=move |ev| {
                set_mode.set(ev.target().value());
            } prop:value=mode>
                <option value="LEXER">"Lexer"</option>
                <option value="PARSER">"Parser"</option>
                <option value="CHECKER">"Checker"</option>
                <option value="EVALUATOR">"Evaluator (Checked)"</option>
                <option value="UNSAFE_EVALUATOR" selected>"Unsafe Evaluator"</option>
            </select>
            <br />
            // {js_code_texturea}
            <strong>"Result:"</strong>
            <pre class="result">{output}</pre>
            <pre class="result error">{error}</pre>
            <r-divider></r-divider>
            <r-collapse label="About" expanded><div>
            "Sena is a modern functional and dependent type programming language base on Rust."<br />"More information can be found at "<a target="_blank" href="https://github.com/BIYUEHU/sena-lang">"https://github.com/BIYUEHU/sena-lang"</a>
            </div></r-collapse>
        </div>
    }
}

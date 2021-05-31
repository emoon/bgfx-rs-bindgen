use bgfx_idl::*;
use convert_case::{Case, Casing};
use std::borrow::Cow;

// list of functions to skip that are manually implemented
static SKIP_FUNCS: &[&str] = &["getShaderUniforms"];
/*
struct VertexLayout {
    data: bgfx::bgfx_vertex_layout_s;
}

impl VertexLayout {
    pub fn new() -> VertexLayout {
        VertexLayout { data: bgfx::bgfx_vertex_layout_s::default(); }
    }

    pub fn begin(&self, render_type: RenderType) -> &self {
        unsafe { bgfx_vertex_layout_begin(self.data as *const void, render_type);
        self
    }

    pub fn add(attrib: Attrib, num: u8, type: AttribType, normalized: bool, as_int: bool) -> &self {
        unsafe { bgfx_vertex_layout_add(self.data as *const void, render_type);
    }

    pub fn has(attrib: Attrib) -> bool {
        unsafe { bgfx_vertex_layout_has(self.data as *const void) }
    }

    pub fn add(attrib: Attrib, num: u8, type: AttribType, normalized: bool, as_int: bool) -> &self {
        unsafe { bgfx_vertex_layout_add(self.data as *const void, render_type);
    }
}
*/

fn generate_rust_comment(text: &str, indent_level: usize) {
    let lines = text.lines();

    for line in lines {
        println!("{:indent$}/// {}", "", line, indent = indent_level);
    }
}

// look for an enum by name and get how many entries it has. This code
// is expected to always work find an entry so will panic if it doesn't
fn get_enum_count(name: &str, idl: &Idl) -> usize {
    for e in &idl.enums {
        if e.name.text == name {
            return e.entries.len();
        }
    }

    panic!("Enum {} wasn't found", name);
}

fn get_rust_primitive_type(s: &str) -> Option<&'static str> {
    match s {
        "char" => Some("s8"),
        "bool" => Some("bool"),
        "uint8_t" => Some("u8"),
        "uint16_t" => Some("u16"),
        "uint32_t" => Some("u32"),
        "uint64_t" => Some("u32"),
        "int8_t" => Some("s8"),
        "int16_t" => Some("s16"),
        "int32_t" => Some("s32"),
        "int64_t" => Some("s32"),
        "float" => Some("f32"),
        "void" => Some("std::os::raw::c_void"),
        _ => None,
    }
}

fn get_rust_array(type_name: &str, count: &str, idl: &Idl) -> String {
    let value_count;

    // found the count for the array
    if let Some(enum_count) = count.find("::Count") {
        value_count = get_enum_count(&count[..enum_count], idl);
    } else {
        if let Ok(v) = count.parse::<usize>() {
            value_count = v;
        } else {
            panic!(
                "Unable to figure out array count for {}:{}",
                type_name, count
            );
        }
    }

    if let Some(rust_type_name) = get_rust_primitive_type(type_name) {
        format!("[{}; {}usize]", rust_type_name, value_count)
    } else {
        // if we didn't find a matching we assemu we can use it directly
        format!("[{}; {}usize]", type_name, value_count)
    }

    // first we try to parse the number if it's a regular number
    // otherwise we need
}

fn get_rust_type(s: &Type, idl: &Idl, is_arg: bool) -> String {
    let mut output_type = String::new();

    if s.is_ref || (s.is_pointer && is_arg) {
        output_type.push('&')
    } else if s.is_pointer {
        output_type.push('*');

        if !s.is_output {
            output_type.push_str("const ");
        }
    }

    if s.is_output {
        output_type.push_str("mut ");
    }

    match &s.var_type {
        VarType::Primitive(name) => {
            output_type.push_str(get_rust_primitive_type(&name).unwrap());
            output_type
        }

        VarType::Struct(name) => {
            output_type.push_str(&name);
            output_type
        }

        VarType::Enum(name) => {
            output_type.push_str(&name);
            output_type
        }

        VarType::Array(type_name, count) => get_rust_array(&type_name, &count, idl),

        _ => panic!("Unable to figure out Rust type for {:#?}", s),
    }
}

fn generate_struct(s: &Func, idl: &Idl) {
    generate_rust_comment(&s.comments, 0);

    println!("#[repr(C)]");
    println!("struct {} {{", s.name.text);

    for e in &s.args {
        generate_rust_comment(&e.name_line.comment, 4);
        println!(
            "    {}: {},",
            e.name_line.text.to_case(Case::Snake),
            get_rust_type(&e.arg_type, idl, false)
        );
    }

    println!("}}");
}

/// Some functions are of type setName (handle, ptr, len) and we special case those
/// as we can use &str directly then given than len is supported as input.
/// Returns true if setName function was generated, otherwise false
fn generate_set_name_func(f: &Func, real_name: &str) -> bool {
    if f.name.text != "setName" {
        return false;
    }

    let handle_type = &f.args[0].type_name;

    println!("    pub fn {}(&self, name: &str) {{", real_name);
    println!(
        "        unsafe {{ bgfx_sys::{}(self.handle, name.ptr(), name.len() as i32) }}",
        real_name
    );
    println!("    }}");

    true
}

/// Select how to generate the function
#[derive(PartialEq)]
enum FunctionMode {
    Method,
    Global,
    Handle,
}

fn get_snake_name(name: &str) -> String {
    let mut name = name.to_case(Case::Snake);
    // special case: 2D gets translated to 2_d and we want 2d

    let len = name.len();
    let mut remove_incorrect_space = false;

    if name.len() >= 3 {
        let bytes = name.as_bytes();
        let nr = bytes[len - 3];
        if (nr >= b'0') && (nr < b'9') {
            remove_incorrect_space = true;
        }
    }

    if remove_incorrect_space {
        name.remove(len - 2);
    }

    name
}

fn get_ffi_call_name(f: &Func) -> String {
    let mut name = String::with_capacity(256);

    name.push_str("bgfx_");
    name.push_str(&f.class.text);
    name.push_str(&f.name.text);

    get_snake_name(&name)
}

/// General function implementation
fn generate_func(f: &Func, idl: &Idl, func_mode: FunctionMode) {
    // if function has been marked for skipping we skip it
    if SKIP_FUNCS.iter().find(|n| *n == &f.name.text).is_some() {
        return;
    }

    let mut func_name = get_snake_name(&f.name.text);
    let indent_level = match func_mode {
        FunctionMode::Method => 4,
        FunctionMode::Handle => 4,
        FunctionMode::Global => 0,
    };

    // find the replacement name
    for t in &f.table {
        if t.name == "cname" {
            func_name = t.str_data.to_owned();
            break;
        }
    }

    generate_rust_comment(&f.comments, indent_level);

    // generate check name function if it is one
    if generate_set_name_func(f, &func_name) {
        return;
    }

    let arg_count = f.args.len();

    match func_mode {
        FunctionMode::Method => {
            if arg_count == 0 {
                print!("    pub fn {}(&self", func_name);
            } else {
                print!("    pub fn {}(&self, ", func_name);
            }
        }

        FunctionMode::Global => print!("pub fn {}(", func_name),
        _ => (),
    }

    let ffi_name = get_ffi_call_name(f);

    let mut call_args = String::with_capacity(256);

    if func_mode == FunctionMode::Handle {
        if arg_count <= 1 {
            print!("    pub fn {}(&self", func_name);
            call_args.push_str("self.handle");
        } else {
            print!("    pub fn {}(&self, ", func_name);
            call_args.push_str("self.handle, ");
        }
    }

    for (i, arg) in f.args.iter().enumerate() {
        // skip first arg in Handle mode
        if func_mode == FunctionMode::Handle && i == 0 {
            continue;
        }
        let arg_name = arg.name_line.text.to_case(Case::Snake);
        if i == arg_count - 1 {
            call_args.push_str(&arg_name);
            print!("{}: {}", arg_name, get_rust_type(&arg.arg_type, idl, true))
        } else {
            call_args.push_str(&arg_name);
            call_args.push_str(", ");
            print!("{}: {}, ", arg_name, get_rust_type(&arg.arg_type, idl, true))
        }
    }

    println!(") {{");

    match func_mode {
        FunctionMode::Method => {
            println!(
                "       {{ bgfx_sys::{}(self as *const c_void, {}) }}",
                ffi_name, call_args
            );
            println!("    }}\n");
        }

        FunctionMode::Handle => {
            println!("       {{ bgfx_sys::{}({}) }}", ffi_name, call_args);
            println!("    }}\n");
        }

        FunctionMode::Global => {
            println!(
                "    unsafe {{ bgfx_sys::{}({}) }}",
                ffi_name, call_args
            );
            println!("}}\n");
        }
    }
}

fn generate_funcs_for_struct(name: &str, idl: &Idl) {
    println!("impl {} {{", name);
    println!("    pub fn new() -> {} {{", name);
    println!("        let t = MaybeUninit::<{}>::zeroed();", name);
    println!("        unsafe {{ t.assume_init(); }}");
    println!("        t");
    println!("    }}\n");

    for f in &idl.funcs {
        if f.class.text == name {
            generate_func(f, idl, FunctionMode::Method);
        }
    }

    println!("}}\n");
}

fn generate_handles(h: &Typedef) {
    // Generate the handle and also generate the wrapper struct that is used in user code
    println!("#[derive(Copy, Clone, Debug)]");
    println!("struct {} {{ ", h.type_line.text);
    println!("    idx: u16,");
    println!("}}\n");

    let len = h.type_line.text.len() - 6;

    println!("#[derive(Copy, Clone, Debug)]");
    println!("pub struct {} {{ ", &h.type_line.text[..len]);
    println!("    handle: {},", h.type_line.text);
    println!("}}\n");
}

// generates implementations for the handle structs
fn generate_handle_impl(idl: &Idl) {
    for h in &idl.handles {
        let h_type = &h.type_line.text;
        let len = h_type.len() - 6;

        println!("impl {} {{ ", &h_type[..len]);

        // find matching handle name for the first argument
        for f in &idl.funcs {
            if !f.class.text.is_empty() {
                continue;
            }

            if (!f.args.is_empty() && &f.args[0].type_name == h_type) || &f.return_name_line.text == h_type {
                generate_func(f, idl, FunctionMode::Handle);
            }
        }

        println!("}}\n");
    }
}

fn main() {
    let data = bgfx_idl::parse_bgfx_idl("/home/emoon/temp/foo.idl").unwrap();

    /*
    for f in &data.funcs {
        if f.return_type.is_pointer {
            println!("{} -> {}", f.name.text, f.return_name_line.text);
        }
    }
    */

    for h in &data.handles {
        generate_handles(&h);
    }

    for s in &data.structs {
        generate_struct(&s, &data);
    }

    generate_handle_impl(&data);

    for s in &data.structs {
        generate_funcs_for_struct(&s.name.text, &data);
    }

    for f in &data.funcs {
        if f.class.text.is_empty() {
            generate_func(&f, &data, FunctionMode::Global);
        }
    }


    println!("{}", MANUAL_IMPL);

    //generate_funcs_for_struct("VertexLayout", &data);
}

// for various reasons here is a list of manual implementations for some functions
static MANUAL_IMPL: &str = "
/// Returns the number of uniforms and uniform handles used inside a shader.
///
/// Notice that only non-predefined uniforms are returned.
pub fn get_shader_uniforms(handle: ShaderHandle, uniforms: &mut [UniformHandle]) -> u16 {
    unsafe { bgfx_sys::bgfx_get_shader_uniforms(handle, uniforms.as_ptr(), uniforms.len() as u16) }
}

/// bgfx-managed buffer of memory.
///
/// It can be created by either copying existing data through [`copy(...)`], or by referencing
/// existing memory directly through [`reference(...)`].
///
/// [`copy(...)`]: #method.copy
/// [`reference(...)`]: #method.reference
pub struct Memory {
    data: *const bgfx_sys::bgfx_memory_t,
}

impl Memory {
    /// Copies the source data into a new bgfx-managed buffer.
    ///
    /// **IMPORTANT:** If this buffer is never passed into a bgfx call, the memory will never be
    /// freed, and will leak.
    #[inline]
    pub fn copy<T>(data: &[T]) -> Memory {
        unsafe {
            let data = bgfx_sys::bgfx_copy(data.as_ptr() as *const c_void,
                                           mem::size_of_val(data) as u32);
            Memory { data }
        }
    }

    /// Creates a reference to the source data for passing into bgfx. When using this constructor
    /// over the `copy` call, no copy will be created. bgfx will read the source memory directly.
    ///
    /// *Note* That the data passed to this function must be keep alive during the whole duration
    /// of the program and is only really recommended for static data unless you know you know
    /// what you are doing. Thus this function is marked as unsafe because of this reason.
    #[inline]
    pub fn unsafe reference<T>(data: &[T]) -> Memory {
        let data = bgfx_sys::bgfx_make_ref(data.as_ptr() as *const c_void,
                                           mem::size_of_val(data) as u32);
        Memory { data }
    }
}

";

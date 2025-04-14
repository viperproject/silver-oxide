pub mod body;
pub mod exp;
mod idx;
pub mod member;
pub mod resource;

pub use idx::*;

fn newline(f: &mut core::fmt::Formatter<'_>) -> std::fmt::Result {
    let indent = f.width().unwrap_or_default();
    write!(f, "\n{: <1$}", "", indent * 2)
}

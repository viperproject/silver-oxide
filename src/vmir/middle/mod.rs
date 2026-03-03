mod body;
mod exp;
mod idx;
mod resource;

pub use body::*;
pub use exp::*;
pub use idx::*;
pub use resource::*;

fn newline(f: &mut core::fmt::Formatter<'_>) -> std::fmt::Result {
    let indent = f.width().unwrap_or_default();
    write!(f, "\n{: <1$}", "", indent * 2)
}

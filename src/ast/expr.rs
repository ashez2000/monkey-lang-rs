#[derive(Debug, Clone)]
pub enum Expr {
    Null,
    Ident,
    Int,
    Prefix,
    Infix,
    Bool,
    If,
    Fn,
    Call,
    String,
    Array,
    Hash,
    Index,
}

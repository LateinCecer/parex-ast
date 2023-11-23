

/// An AST element is basically everything in the code that lies on the surface of the program.
/// This includes:
///
/// - structs
/// - traits
/// - impls
/// - functions
///
#[derive(Debug, Clone)]
pub enum AstElement {
    Struct,
    Enum,
    Trait,
    Impl,
    Function,
}


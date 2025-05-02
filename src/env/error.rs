#[derive(Debug, Clone)]
pub enum EnvError {
    RedefinedBinding(String),
}

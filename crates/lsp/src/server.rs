pub struct LspServer {}

impl Default for LspServer {
    fn default() -> Self {
        Self::new()
    }
}

impl LspServer {
    pub fn new() -> Self {
        Self {}
    }

    pub fn start(&self) {}
}

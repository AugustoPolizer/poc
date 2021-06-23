use crate::parser::errors::{
    internal_error_msg_handle, InternalError, InternalErrorTypes, ParsingError,
};

use std::collections;

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    INTEGER,
    FLOAT,
    STRING,
}

#[derive(Clone)]
pub struct Symbol {
    symbol_type: Type,
    is_const: bool,
}

impl Symbol {
    pub fn new(symbol_type: Type, is_const: bool) -> Symbol {
        Symbol {
            symbol_type,
            is_const,
        }
    }
}

#[derive(Clone)]
pub struct FuncDecl {
    return_type: Type,
    params: Vec<Symbol>,
}

impl FuncDecl {
    pub fn new(return_type: Type, params: Vec<Symbol>) -> FuncDecl {
        FuncDecl {
            return_type,
            params,
        }
    }
}

pub struct Scope {
    symbol_table: collections::HashMap<String, Symbol>,
    function_table: collections::HashMap<String, FuncDecl>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            symbol_table: collections::HashMap::new(),
            function_table: collections::HashMap::new(),
        }
    }
}

pub struct ScopeManager {
    scopes: Vec<Scope>,
}

impl ScopeManager {
    pub fn new() -> ScopeManager {
        ScopeManager { scopes: Vec::new() }
    }

    pub fn create_new_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    pub fn remove_scope(&mut self) -> Option<Scope> {
        match self.scopes.pop() {
            Some(s) => Some(s),
            None => None,
        }
    }

    pub fn create_new_symbol(
        &mut self,
        symbol_type: Type,
        symbol_name: &str,
        is_const: bool,
    ) -> Result<(), ParsingError> {
        if self.scopes.is_empty() {
            return Err(ParsingError::Internal(InternalError::new(
                internal_error_msg_handle(
                    InternalErrorTypes::UNABLETOINSERTSYMBOL,
                    "There is no scope to insert the symbol",
                ),
            )));
        }

        let idx = self.scopes.len() - 1;
        self.scopes[idx]
            .symbol_table
            .insert(symbol_name.to_string(), Symbol::new(symbol_type, is_const));

        Ok(())
    }

    pub fn insert_func_decl(
        &mut self,
        func_decl: FuncDecl,
        func_name: String,
    ) -> Result<(), &'static str> {
        if self.scopes.is_empty() {
            return Err("There is no scope to insert function declaration");
        }

        let last_scope_index = self.scopes.len() - 1;
        self.scopes[last_scope_index]
            .function_table
            .insert(func_name, func_decl);

        Ok(())
    }

    pub fn find_symbol_in_current_scope(&self, name: &str) -> Option<&Symbol> {
        if self.scopes.is_empty() {
            return None;
        }

        let last_scope_index = self.scopes.len() - 1;
        match self.scopes[last_scope_index].symbol_table.get(name) {
            Some(s) => Some(s),
            None => None,
        }
    }

    pub fn find_symbol(&self, name: &str) -> Option<&Symbol> {
        let iter = self.scopes.iter().rev();
        for scope in iter {
            match scope.symbol_table.get(name) {
                Some(s) => return Some(s),
                None => (),
            };
        }
        None
    }

    pub fn find_func_decl_in_current_scope(&self, name: &str) -> Option<&FuncDecl> {
        if self.scopes.is_empty() {
            return None;
        }

        let last_scope_index = self.scopes.len() - 1;
        match self.scopes[last_scope_index].function_table.get(name) {
            Some(s) => Some(s),
            None => None,
        }
    }

    pub fn find_func_decl(&self, name: &str) -> Option<&FuncDecl> {
        let iter = self.scopes.iter().rev();

        for scope in iter {
            match scope.function_table.get(name) {
                Some(f) => return Some(f),
                None => (),
            };
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_symbol_equal(symbol: &Symbol, symbol_type: Type, is_const: bool) {
        assert_eq!(symbol.symbol_type, symbol_type);
        assert_eq!(symbol.is_const, is_const);
    }

    #[test]
    fn create_scope() {
        let mut scope_manager = ScopeManager::new();

        scope_manager.create_new_scope();
        match scope_manager.remove_scope() {
            Some(_) => assert!(true),
            None => assert!(false),
        };
    }

    #[test]
    fn remove_scope() {
        let mut scope_manager = ScopeManager::new();

        scope_manager.create_new_scope();
        match scope_manager.remove_scope() {
            Some(_) => match scope_manager.remove_scope() {
                Some(_) => assert!(false),
                None => assert!(true),
            },
            None => assert!(false),
        };
    }

    #[test]
    fn insert_symbol_correct_use() {
        let mut scope_manager = ScopeManager::new();

        scope_manager.create_new_scope();
        match scope_manager.create_new_symbol(Type::INTEGER, "var_test", false) {
            Ok(_) => assert!(true),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn insert_symbol_incorrect_use() {
        let mut scope_manager = ScopeManager::new();

        match scope_manager.create_new_symbol(Type::INTEGER, "var_test", false) {
            Ok(_) => assert!(false),
            Err(_) => assert!(true),
        }
    }

    #[test]
    fn insert_func_decl_correct_use() {
        let mut scope_manager = ScopeManager::new();

        scope_manager.create_new_scope();
        match scope_manager.insert_func_decl(
            FuncDecl::new(Type::INTEGER, Vec::new()),
            String::from("test_function"),
        ) {
            Ok(_) => assert!(true),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn insert_func_decl_incorrect_use() {
        let mut scope_manager = ScopeManager::new();

        match scope_manager.insert_func_decl(
            FuncDecl::new(Type::INTEGER, Vec::new()),
            String::from("test_function"),
        ) {
            Ok(_) => assert!(false),
            Err(_) => assert!(true),
        }
    }

    #[test]
    fn find_symbol_in_current_scope() {
        let mut scope_manager = ScopeManager::new();

        scope_manager.create_new_scope();
        match scope_manager.create_new_symbol(Type::INTEGER, "var_test1", true) {
            Ok(_) => assert!(true),
            Err(_) => assert!(false),
        }

        scope_manager.create_new_scope();
        match scope_manager.create_new_symbol(Type::FLOAT, "var_test2", false) {
            Ok(_) => assert!(true),
            Err(_) => assert!(false),
        }

        match scope_manager.find_symbol_in_current_scope("var_test1") {
            Some(_) => assert!(false),
            None => assert!(true),
        };

        let symbol = match scope_manager.find_symbol_in_current_scope("var_test2") {
            Some(x) => x,
            None => return assert!(false),
        };

        assert_eq!(symbol.symbol_type, Type::FLOAT);
        assert_eq!(symbol.is_const, false);
    }

    #[test]
    fn find_symbol_with_only_one_scope() {
        let mut scope_manager = ScopeManager::new();

        scope_manager.create_new_scope();
        match scope_manager.create_new_symbol(Type::INTEGER, "var_test", true) {
            Ok(_) => assert!(true),
            Err(_) => assert!(false),
        }
        let symbol = match scope_manager.find_symbol("var_test") {
            Some(x) => x,
            None => return assert!(false),
        };
        assert_symbol_equal(symbol, Type::INTEGER, true)
    }

    #[test]
    fn find_symbol_with_two_scopes() {
        let mut scope_manager = ScopeManager::new();

        scope_manager.create_new_scope();
        match scope_manager.create_new_symbol(Type::INTEGER, "var_test1", true) {
            Ok(_) => assert!(true),
            Err(_) => assert!(false),
        }

        scope_manager.create_new_scope();
        match scope_manager.create_new_symbol(Type::FLOAT, "var_test2", false) {
            Ok(_) => assert!(true),
            Err(_) => assert!(false),
        }

        let mut symbol = match scope_manager.find_symbol("var_test2") {
            Some(x) => x,
            None => return assert!(false),
        };
        assert_symbol_equal(&symbol, Type::FLOAT, false);

        scope_manager.remove_scope();
        match scope_manager.find_symbol("var_test2") {
            Some(_) => assert!(false),
            None => assert!(true),
        };

        symbol = match scope_manager.find_symbol("var_test1") {
            Some(x) => x,
            None => return assert!(false),
        };
        assert_symbol_equal(symbol, Type::INTEGER, true);

        scope_manager.remove_scope();
        match scope_manager.find_symbol("var_test1") {
            Some(_) => assert!(false),
            None => assert!(true),
        };
    }

    #[test]
    fn find_func_decl_with_only_one_scope() {
        let mut scope_manager = ScopeManager::new();

        scope_manager.create_new_scope();
        match scope_manager.insert_func_decl(
            FuncDecl::new(Type::INTEGER, Vec::new()),
            String::from("test_function"),
        ) {
            Ok(_) => assert!(true),
            Err(_) => assert!(false),
        }
        let func_decl = match scope_manager.find_func_decl("test_function") {
            Some(x) => x,
            None => return assert!(false),
        };
        assert_eq!(func_decl.return_type, Type::INTEGER);
    }

    #[test]
    fn find_func_decl_with_two_scopes() {
        let mut scope_manager = ScopeManager::new();

        scope_manager.create_new_scope();
        match scope_manager.insert_func_decl(
            FuncDecl::new(Type::INTEGER, Vec::new()),
            String::from("test_function1"),
        ) {
            Ok(_) => assert!(true),
            Err(_) => assert!(false),
        }

        scope_manager.create_new_scope();
        match scope_manager.insert_func_decl(
            FuncDecl::new(Type::FLOAT, Vec::new()),
            String::from("test_function2"),
        ) {
            Ok(_) => assert!(true),
            Err(_) => assert!(false),
        }

        let mut func_decl = match scope_manager.find_func_decl("test_function2") {
            Some(x) => x,
            None => return assert!(false),
        };
        assert_eq!(func_decl.return_type, Type::FLOAT);

        scope_manager.remove_scope();
        match scope_manager.find_func_decl("test_function2") {
            Some(_) => assert!(false),
            None => assert!(true),
        };

        func_decl = match scope_manager.find_func_decl("test_function1") {
            Some(x) => x,
            None => return assert!(false),
        };
        assert_eq!(func_decl.return_type, Type::INTEGER);

        scope_manager.remove_scope();
        match scope_manager.find_func_decl("test_function1") {
            Some(_) => assert!(false),
            None => assert!(true),
        };
    }
}

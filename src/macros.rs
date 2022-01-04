/*!
One-line description.

More detailed description, with

# Example

*/

// ------------------------------------------------------------------------------------------------
// Public Macros
// ------------------------------------------------------------------------------------------------

macro_rules! self_is_as {
    ($fn_root:ident, $variant_and_type:ident) => {
        self_is_as!($fn_root, $variant_and_type, $variant_and_type);
    };
    ($fn_root:ident, $variant:ident, $value_type:ident) => {
        paste! {
            pub fn [<is_ $fn_root>](&self) -> bool {
                matches!(self, Self::$variant(_))
            }

            pub fn [<as_ $fn_root>](&self) -> Option<&$value_type> {
                match self {
                    Self::$variant(v) => Some(v),
                    _ => None,
                }
            }
        }
    };
}

macro_rules! get {
    ($fn_and_field_name:ident -> $type_name:ty) => {
        pub fn $fn_and_field_name(&self) -> &$type_name {
            &self.$fn_and_field_name
        }
    };
    ($fn_name:ident, $field_name:ident -> $type_name:ty) => {
        pub fn $fn_name(&self) -> &$type_name {
            &self.$field_name
        }
    };
}

macro_rules! delegate {
    ($fn_name:ident) => {
        delegate!($fn_name, 0);
    };
    ($fn_name:ident, $inner:ident) => {
        paste! {
            pub fn $fn_name(&self) {
                self.$inner.$fn_name()
            }
        }
    };
    ($fn_name:ident, $inner:expr) => {
        paste! {
            pub fn $fn_name(&self) {
                self.$inner.$fn_name()
            }
        }
    };
    ($fn_name:ident -> $return_type:ty) => {
        delegate!($fn_name, 0, $return_type);
    };
    ($fn_name:ident, $inner:ident -> $return_type:ty) => {
        paste! {
            pub fn $fn_name(&self) -> $return_type {
                self.$inner.$fn_name()
            }
        }
    };
    ($fn_name:ident, $inner:expr, $return_type:ty) => {
        paste! {
            pub fn $fn_name(&self) -> $return_type {
                self.$inner.$fn_name()
            }
        }
    };
}

#[allow(unused_macros)]
macro_rules! delegate_mut {
    ($fn_name:ident) => {
        delegate_mut!($fn_name, 0);
    };
    ($fn_name:ident, $inner:ident) => {
        paste! {
            pub fn $fn_name(&mut self) {
                self.$inner.$fn_name()
            }
        }
    };
    ($fn_name:ident, $inner:expr) => {
        paste! {
            pub fn $fn_name(&mut self) {
                self.$inner.$fn_name()
            }
        }
    };
    ($fn_name:ident -> $return_type:ty) => {
        delegate!($fn_name, 0, $return_type);
    };
    ($fn_name:ident, $inner:ident -> $return_type:ty) => {
        paste! {
            pub fn $fn_name(&mut self) -> $return_type {
                self.$inner.$fn_name()
            }
        }
    };
    ($fn_name:ident, $inner:expr, $return_type:ty) => {
        paste! {
            pub fn $fn_name(&mut self) -> $return_type {
                self.$inner.$fn_name()
            }
        }
    };
}

// ------------------------------------------------------------------------------------------------
// Private Types & Constants
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Private Macros
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Public Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

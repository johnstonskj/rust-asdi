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
        self_is_as!(pub $fn_root, $variant_and_type, $variant_and_type);
    };
    ($vis:vis $fn_root:ident, $variant_and_type:ident) => {
        self_is_as!($fn_root, $variant_and_type, $variant_and_type);
    };
    ($fn_root:ident, $variant:ident, $value_type:ident) => {
        self_is_as!(pub $fn_root, $variant, $value_type);
    };
    // --------------------------------------------------------------------------------------------
    ($vis:vis $fn_root:ident, $variant:ident, $value_type:ident) => {
        paste! {
            $vis fn [<is_ $fn_root>](&self) -> bool {
                matches!(self, Self::$variant(_))
            }

            $vis fn [<as_ $fn_root>](&self) -> Option<&$value_type> {
                match self {
                    Self::$variant(v) => Some(v),
                    _ => None,
                }
            }
        }
    };
}

macro_rules! get {
    ($vis:vis $fn_and_field_name:ident -> $type_name:ty) => {
        get!($vis $fn_and_field_name, $fn_and_field_name -> $type_name);
    };
    // --------------------------------------------------------------------------------------------
    ($vis:vis $fn_name:ident, $field_name:ident -> $type_name:ty) => {
        $vis fn $fn_name(&self) -> &$type_name {
            &self.$field_name
        }
    };
}

macro_rules! get_cloned {
    ($vis:vis $fn_and_field_name:ident -> $type_name:ty) => {
        get_cloned!($vis $fn_and_field_name, $fn_and_field_name -> $type_name)
    };
    // --------------------------------------------------------------------------------------------
    ($vis:vis $fn_name:ident, $field_name:ident -> $type_name:ty) => {
        $vis fn $fn_name(&self) -> $type_name {
            self.$field_name.clone()
        }
    };
}

macro_rules! delegate {
    ($vis:vis $fn_name:ident) => {
        $vis fn $fn_name(&self) {
            self.0.$fn_name()
        }
    };
    ($vis:vis $fn_name:ident, $inner:ident) => {
        $vis fn $fn_name(&self) {
            self.$inner.$fn_name()
        }
    };
    // --------------------------------------------------------------------------------------------
    ($vis:vis $fn_name:ident -> $return_type:ty) => {
        $vis fn $fn_name(&self) -> $return_type {
            self.0.$fn_name()
        }
    };
    ($vis:vis $fn_name:ident, $inner:ident -> $return_type:ty) => {
        $vis fn $fn_name(&self) -> $return_type {
            self.$inner.$fn_name()
        }
    };
}

macro_rules! impl_labeled {
    ($for_type:ty) => {
        impl Labeled for $for_type {
            get!(label -> Predicate);

            get_cloned!(label_ref, label -> PredicateRef);
        }
    };
    ($for_type:ty, $field_name:ident) => {
        impl Labeled for $for_type {
            get!(label, $field_name -> Predicate);

            get_cloned!(label_ref, $field_name -> PredicateRef);
        }
    };
}

macro_rules! impl_collection {
    ($for_type:ty, $type_t:ty, $inner:ident) => {
        impl Collection<$type_t> for $for_type {
            delegate!(is_empty, $inner -> bool);

            delegate!(len, $inner -> usize);

            fn iter(&self) -> Box<dyn Iterator<Item = &'_ $type_t> + '_> {
                Box::new(self.$inner.iter())
            }

            fn contains(&self, value: &$type_t) -> bool {
                self.$inner.contains(value)
            }
        }
    };
}

macro_rules! impl_indexed_collection {
    ($for_type:ty, $type_t:ty, $type_i:ty, $inner:ident) => {
        impl IndexedCollection<$type_i, $type_t> for $for_type {
            fn get(&self, index: &$type_i) -> Option<&$type_t> {
                self.$inner.get(*index)
            }

            fn contains_index(&self, index: &$type_i) -> bool {
                *index < self.len()
            }
        }
    };
}

macro_rules! impl_enum_from {
    ($outer:ty, $inner:ty, $variant:ident) => {
        impl From<$inner> for $outer {
            fn from(v: $inner) -> Self {
                Self::$variant(v)
            }
        }
    };
}

macro_rules! into_inner_fn {
    ($vis:vis $inner_type:ty) => {
        pub fn into_inner(self) -> $inner_type {
            self.0
        }
    };
    ($vis:vis $inner_type:ty, $field_name:ident) => {
        pub fn into_inner(self) -> $inner_type {
            self.$field_name
        }
    };
}

macro_rules! const_value {
    ($name:ident: $kind:ty => $value:expr) => {
        const_value!(pub(crate) $name: $kind => $value);
    };
    ($vis:vis $name:ident: $kind:ty => $value:expr) => {
        $vis const $name: $kind = $value;
    };
}

macro_rules! const_string {
    ($name:ident => $value:expr) => {
        const_string!(pub(crate) $name => $value);
    };
    ($vis:vis $name:ident => $value:expr) => {
        const_value!($vis $name: &str => $value);
    };
}

macro_rules! const_char {
    ($name:ident => $value:expr) => {
        const_char!(pub(crate) $name => $value);
    };
    ($vis:vis $name:ident => $value:expr) => {
        const_value!($vis $name: char => $value);
    };
}

macro_rules! const_string_block {
    ($vis:vis $prefix:ident $(($name:ident => $value:expr)),+) => {
        $(
            paste! {const_string!([<$prefix _ $name>] => $value); }
        )+
    };
}

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    DeriveInput, Ident, LitStr, Token,
};

use crate::util::{import_ruma_common, to_camel_case};

/// Create an `AccountDataContent` implementation for a struct.
pub fn expand_account_data_content(
    input: &DeriveInput,
    ruma_common: &TokenStream,
) -> syn::Result<TokenStream> {
    let content_attr = input
        .attrs
        .iter()
        .filter(|attr| attr.path.is_ident("account_data"))
        .map(|attr| attr.parse_args::<MetaAttrs>())
        .collect::<syn::Result<Vec<_>>>()?;

    let mut data_types: Vec<_> =
        content_attr.iter().filter_map(|attrs| attrs.find_type()).collect();
    let data_type = match data_types.as_slice() {
        [] => {
            return Err(syn::Error::new(
                Span::call_site(),
                "no type attribute found, please add
                 `#[account_data(type = \"m.someting\")]`",
            ));
        }
        [_] => data_types.pop().unwrap(),
        _ => {
            return Err(syn::Error::new(
                Span::call_site(),
                "multiple type attribute found, there can only be one",
            ));
        }
    };

    let mut kinds: Vec<_> = content_attr.iter().filter_map(|attrs| attrs.find_kind()).collect();
    let kind = match kinds.as_slice() {
        [] => {
            return Err(syn::Error::new(
                Span::call_site(),
                "no kind attribute found, please add
                 `#[account_data(kind = Kind)]`",
            ));
        }
        [_] => Some(kinds.pop().unwrap()),
        _ => {
            return Err(syn::Error::new(
                Span::call_site(),
                "multiple kind attribute found, there can only be one",
            ));
        }
    };

    let main_impl = generate_account_data_content_impl(&input.ident, data_type, ruma_common);
    let marker_trait_impl =
        kind.map(|k| generate_marker_trait_impl(k, &input.ident, ruma_common)).transpose()?;
    let type_aliases = kind
        .map(|k| {
            generate_account_data_type_aliases(k, &input.ident, &data_type.value(), ruma_common)
        })
        .transpose()?;

    Ok(quote! {
        #main_impl
        #marker_trait_impl
        #type_aliases
    })
}

mod kw {
    syn::custom_keyword!(kind);
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AccountDataKind {
    Global,
    Room,
}

impl AccountDataKind {
    fn to_struct_name(self) -> TokenStream {
        match self {
            Self::Global => quote! { GlobalAccountData },
            Self::Room => quote! { RoomAccountData },
        }
    }
}

impl Parse for AccountDataKind {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;
        Ok(match ident.to_string().as_str() {
            "Global" => AccountDataKind::Global,
            "Room" => AccountDataKind::Room,
            id => {
                return Err(syn::Error::new_spanned(
                    ident,
                    format!("valid account data kinds are Global and Room, found `{}`", id),
                ));
            }
        })
    }
}

/// Parses attributes for `AccountDataContent` derives.
///
/// `#[account_data(type = "m.room.alias", kind = Kind)]`
enum AccountDataMeta {
    Type(LitStr),
    Kind(AccountDataKind),
}

impl AccountDataMeta {
    fn data_type(&self) -> Option<&LitStr> {
        match self {
            Self::Type(t) => Some(t),
            _ => None,
        }
    }

    fn data_kind(&self) -> Option<AccountDataKind> {
        match self {
            Self::Kind(k) => Some(*k),
            _ => None,
        }
    }
}

impl Parse for AccountDataMeta {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![type]) {
            let _: Token![type] = input.parse()?;
            let _: Token![=] = input.parse()?;
            input.parse().map(AccountDataMeta::Type)
        } else if lookahead.peek(kw::kind) {
            let _: kw::kind = input.parse()?;
            let _: Token![=] = input.parse()?;
            AccountDataKind::parse(input).map(AccountDataMeta::Kind)
        } else {
            Err(lookahead.error())
        }
    }
}

struct MetaAttrs(Vec<AccountDataMeta>);

impl MetaAttrs {
    fn find_type(&self) -> Option<&LitStr> {
        self.0.iter().find_map(|a| a.data_type())
    }

    fn find_kind(&self) -> Option<AccountDataKind> {
        self.0.iter().find_map(|a| a.data_kind())
    }
}

impl Parse for MetaAttrs {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let attrs =
            syn::punctuated::Punctuated::<AccountDataMeta, Token![,]>::parse_terminated(input)?;
        Ok(Self(attrs.into_iter().collect()))
    }
}

fn generate_account_data_content_impl(
    ident: &Ident,
    data_type: &LitStr,
    ruma_common: &TokenStream,
) -> TokenStream {
    let serde = quote! { #ruma_common::exports::serde };
    let serde_json = quote! { #ruma_common::exports::serde_json };

    quote! {
        #[automatically_derived]
        impl #ruma_common::account_data::AccountDataContent for #ident {
            fn data_type(&self) -> &str {
                #data_type
            }

            fn from_parts(
                ev_type: &str,
                content: &#serde_json::value::RawValue,
            ) -> #serde_json::Result<Self> {
                if ev_type != #data_type {
                    return Err(#serde::de::Error::custom(
                        format!("expected type `{}`, found `{}`", #data_type, ev_type)
                    ));
                }

                #serde_json::from_str(content.get())
            }
        }
    }
}

fn generate_account_data_type_aliases(
    data_kind: AccountDataKind,
    ident: &Ident,
    data_type: &str,
    ruma_common: &TokenStream,
) -> syn::Result<TokenStream> {
    let alias = Ident::new(
        ident.to_string().strip_suffix("Content").ok_or_else(|| {
            syn::Error::new_spanned(ident, "Expected content struct name ending in `Content`")
        })?,
        Span::call_site(),
    );

    let generic_struct = data_kind.to_struct_name();
    let alias_docs = format!("An `{}` object.", data_type);

    Ok(quote! {
        #[doc = #alias_docs]
        pub type #alias = #ruma_common::account_data::#generic_struct<#ident>;
    })
}

fn generate_marker_trait_impl(
    data_kind: AccountDataKind,
    ident: &Ident,
    ruma_common: &TokenStream,
) -> syn::Result<TokenStream> {
    let marker_trait = match data_kind {
        AccountDataKind::Global => quote! { GlobalAccountDataContent },
        AccountDataKind::Room => quote! { RoomAccountDataContent },
    };

    Ok(quote! {
        #[automatically_derived]
        impl #ruma_common::account_data::#marker_trait for #ident {}
    })
}

// AccountData derive

/// Derive `Event` macro code generation.
pub fn expand_account_data(input: DeriveInput) -> syn::Result<TokenStream> {
    let ruma_common = import_ruma_common();

    let ident = &input.ident;
    let (kind, var) = to_kind_variation(ident).ok_or_else(|| {
        syn::Error::new_spanned(ident, "not a valid ruma event struct identifier")
    })?;

    let fields: Vec<_> = if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { named, .. }),
        ..
    }) = &input.data
    {
        if !named.iter().any(|f| f.ident.as_ref().unwrap() == "content") {
            return Err(syn::Error::new(
                Span::call_site(),
                "struct must contain a `content` field",
            ));
        }

        named.iter().cloned().collect()
    } else {
        return Err(syn::Error::new_spanned(
            input.ident,
            "the `Event` derive only supports structs with named fields",
        ));
    };

    let mut res = TokenStream::new();

    res.extend(expand_serialize_event(&input, var, &fields, &ruma_common));
    res.extend(expand_deserialize_event(&input, kind, var, &fields, &ruma_common)?);

    if var.is_sync() {
        res.extend(expand_sync_from_into_full(&input, kind, var, &fields, &ruma_common));
    }

    Ok(res)
}

fn expand_serialize_event(
    input: &DeriveInput,
    fields: &[Field],
    ruma_common: &TokenStream,
) -> TokenStream {
    let serde = quote! { #ruma_common::exports::serde };

    let ident = &input.ident;
    let (impl_gen, ty_gen, where_clause) = input.generics.split_for_impl();
    let serialize_fields: Vec<_> = fields
        .iter()
        .map(|field| {
            let name = field.ident.as_ref().unwrap();
            if name == "content" && var.is_redacted() {
                quote! {
                    if #ruma_common::events::RedactedEventContent::has_serialize_fields(&self.content) {
                        state.serialize_field("content", &self.content)?;
                    }
                }
            } else if name == "unsigned" {
                quote! {
                    if !self.unsigned.is_empty() {
                        state.serialize_field("unsigned", &self.unsigned)?;
                    }
                }
            } else {
                let name_s = name.to_string();
                match &field.ty {
                    syn::Type::Path(syn::TypePath { path: syn::Path { segments, .. }, .. })
                        if segments.last().unwrap().ident == "Option" =>
                    {
                        quote! {
                            if let Some(content) = self.#name.as_ref() {
                                state.serialize_field(#name_s, content)?;
                            }
                        }
                    }
                    _ => quote! {
                        state.serialize_field(#name_s, &self.#name)?;
                    },
                }
            }
        })
        .collect();

    quote! {
        #[automatically_derived]
        impl #impl_gen #serde::ser::Serialize for #ident #ty_gen #where_clause {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: #serde::ser::Serializer,
            {
                use #serde::ser::{SerializeStruct as _, Error as _};

                let event_type = #ruma_common::events::EventContent::event_type(&self.content);

                let mut state = serializer.serialize_struct(stringify!(#ident), 7)?;

                state.serialize_field("type", event_type)?;
                #( #serialize_fields )*
                state.end()
            }
        }
    }
}

fn expand_deserialize_event(
    input: &DeriveInput,
    _kind: EventKind,
    fields: &[Field],
    ruma_common: &TokenStream,
) -> syn::Result<TokenStream> {
    let serde = quote! { #ruma_common::exports::serde };
    let serde_json = quote! { #ruma_common::exports::serde_json };

    let ident = &input.ident;
    // we know there is a content field already
    let content_type = &fields
        .iter()
        // we also know that the fields are named and have an ident
        .find(|f| f.ident.as_ref().unwrap() == "content")
        .unwrap()
        .ty;

    let (impl_generics, ty_gen, where_clause) = input.generics.split_for_impl();
    let is_generic = !input.generics.params.is_empty();

    let enum_variants: Vec<_> = fields
        .iter()
        .map(|field| {
            let name = field.ident.as_ref().unwrap();
            to_camel_case(name)
        })
        .collect();

    let field_names: Vec<_> = fields.iter().flat_map(|f| &f.ident).collect();

    let deserialize_impl_gen = if is_generic {
        let gen = &input.generics.params;
        quote! { <'de, #gen> }
    } else {
        quote! { <'de> }
    };
    let deserialize_phantom_type = if is_generic {
        quote! { ::std::marker::PhantomData }
    } else {
        quote! {}
    };

    Ok(quote! {
        #[automatically_derived]
        impl #deserialize_impl_gen #serde::de::Deserialize<'de> for #ident #ty_gen #where_clause {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: #serde::de::Deserializer<'de>,
            {
                #[derive(#serde::Deserialize)]
                #[serde(field_identifier, rename_all = "snake_case")]
                enum Field {
                    // since this is represented as an enum we have to add it so the JSON picks it
                    // up
                    Type,
                    #( #enum_variants, )*
                    #[serde(other)]
                    Unknown,
                }

                /// Visits the fields of an event struct to handle deserialization of
                /// the `content` and `prev_content` fields.
                struct EventVisitor #impl_generics (#deserialize_phantom_type #ty_gen);

                #[automatically_derived]
                impl #deserialize_impl_gen #serde::de::Visitor<'de>
                    for EventVisitor #ty_gen #where_clause
                {
                    type Value = #ident #ty_gen;

                    fn expecting(
                        &self,
                        formatter: &mut ::std::fmt::Formatter<'_>,
                    ) -> ::std::fmt::Result {
                        write!(formatter, "struct implementing {}", stringify!(#content_type))
                    }

                    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
                    where
                        A: #serde::de::MapAccess<'de>,
                    {
                        use #serde::de::Error as _;

                        let mut event_type: ::std::option::Option<::std::string::String> =
                            ::std::option::Option::None;
                        let mut content: ::std::option::Option<
                            ::std::boxed::Box<#serde_json::value::RawValue>,
                        > = ::std::option::Option::None;

                        while let Some(key) = map.next_key()? {
                            match key {
                                Field::Unknown => {
                                    let _: #serde::de::IgnoredAny = map.next_value()?;
                                },
                                Field::Type => {
                                    if event_type.is_some() {
                                        return Err(#serde::de::Error::duplicate_field("type"));
                                    }
                                    event_type = Some(map.next_value()?);
                                }
                                #(
                                    Field::#enum_variants => {
                                        if #field_names.is_some() {
                                            return Err(#serde::de::Error::duplicate_field(
                                                stringify!(#field_names),
                                            ));
                                        }
                                        #field_names = Some(map.next_value()?);
                                    }
                                )*
                            }
                        }

                        let event_type =
                            event_type.ok_or_else(|| #serde::de::Error::missing_field("type"))?;
                        let content = {
                            let json = content
                                .ok_or_else(|| #serde::de::Error::missing_field("content"))?;
                            C::from_parts(&event_type, &json).map_err(A::Error::custom)?
                        };

                        Ok(#ident {
                            #( #field_names ),*
                        })
                    }
                }

                deserializer.deserialize_map(EventVisitor(#deserialize_phantom_type))
            }
        }
    })
}

fn expand_redact_event(
    input: &DeriveInput,
    kind: EventKind,
    var: EventKindVariation,
    fields: &[Field],
    ruma_common: &TokenStream,
) -> TokenStream {
    let ruma_identifiers = quote! { #ruma_common::exports::ruma_identifiers };

    let redacted_type = kind.to_event_ident(var.to_redacted());
    let redacted_content_trait =
        format_ident!("{}Content", kind.to_event_ident(EventKindVariation::Redacted));
    let ident = &input.ident;

    let mut generics = input.generics.clone();
    if generics.params.is_empty() {
        return TokenStream::new();
    }

    assert_eq!(generics.params.len(), 1, "expected one generic parameter");
    let ty_param = match &generics.params[0] {
        GenericParam::Type(ty) => ty.ident.clone(),
        _ => panic!("expected a type parameter"),
    };

    let where_clause = generics.make_where_clause();
    where_clause.predicates.push(parse_quote! { #ty_param: #ruma_common::events::RedactContent });
    where_clause.predicates.push(parse_quote! {
        <#ty_param as #ruma_common::events::RedactContent>::Redacted:
            #ruma_common::events::#redacted_content_trait
    });

    let (impl_generics, ty_gen, where_clause) = generics.split_for_impl();

    let fields = fields.iter().filter_map(|field| {
        let ident = field.ident.as_ref().unwrap();

        if ident == "content" || ident == "prev_content" {
            None
        } else if ident == "unsigned" {
            Some(quote! {
                unsigned: #ruma_common::events::RedactedUnsigned::new_because(
                    ::std::boxed::Box::new(redaction),
                )
            })
        } else {
            Some(quote! {
                #ident: self.#ident
            })
        }
    });

    quote! {
        #[automatically_derived]
        impl #impl_generics #ruma_common::events::Redact for #ident #ty_gen #where_clause {
            type Redacted = #ruma_common::events::#redacted_type<
                <#ty_param as #ruma_common::events::RedactContent>::Redacted,
            >;

            fn redact(
                self,
                redaction: #ruma_common::events::room::redaction::SyncRoomRedactionEvent,
                version: &#ruma_identifiers::RoomVersionId,
            ) -> Self::Redacted {
                let content = #ruma_common::events::RedactContent::redact(self.content, version);
                #ruma_common::events::#redacted_type {
                    content,
                    #(#fields),*
                }
            }
        }
    }
}

fn expand_sync_from_into_full(
    input: &DeriveInput,
    kind: EventKind,
    var: EventKindVariation,
    fields: &[Field],
    ruma_common: &TokenStream,
) -> TokenStream {
    let ruma_identifiers = quote! { #ruma_common::exports::ruma_identifiers };

    let ident = &input.ident;
    let full_struct = kind.to_event_ident(var.to_full());
    let (impl_generics, ty_gen, where_clause) = input.generics.split_for_impl();
    let fields: Vec<_> = fields.iter().flat_map(|f| &f.ident).collect();

    quote! {
        #[automatically_derived]
        impl #impl_generics ::std::convert::From<#full_struct #ty_gen>
            for #ident #ty_gen #where_clause
        {
            fn from(event: #full_struct #ty_gen) -> Self {
                let #full_struct { #( #fields, )* .. } = event;
                Self { #( #fields, )* }
            }
        }

        #[automatically_derived]
        impl #impl_generics #ident #ty_gen #where_clause {
            /// Convert this sync event into a full event, one with a room_id field.
            pub fn into_full_event(
                self,
                room_id: ::std::boxed::Box<#ruma_identifiers::RoomId>,
            ) -> #full_struct #ty_gen {
                let Self { #( #fields, )* } = self;
                #full_struct {
                    #( #fields, )*
                    room_id,
                }
            }
        }
    }
}

fn expand_eq_ord_event(input: &DeriveInput) -> TokenStream {
    let ident = &input.ident;
    let (impl_gen, ty_gen, where_clause) = input.generics.split_for_impl();

    quote! {
        #[automatically_derived]
        impl #impl_gen ::std::cmp::PartialEq for #ident #ty_gen #where_clause {
            /// Checks if two `EventId`s are equal.
            fn eq(&self, other: &Self) -> ::std::primitive::bool {
                self.event_id == other.event_id
            }
        }

        #[automatically_derived]
        impl #impl_gen ::std::cmp::Eq for #ident #ty_gen #where_clause {}

        #[automatically_derived]
        impl #impl_gen ::std::cmp::PartialOrd for #ident #ty_gen #where_clause {
            /// Compares `EventId`s and orders them lexicographically.
            fn partial_cmp(&self, other: &Self) -> ::std::option::Option<::std::cmp::Ordering> {
                self.event_id.partial_cmp(&other.event_id)
            }
        }

        #[automatically_derived]
        impl #impl_gen ::std::cmp::Ord for #ident #ty_gen #where_clause {
            /// Compares `EventId`s and orders them lexicographically.
            fn cmp(&self, other: &Self) -> ::std::cmp::Ordering {
                self.event_id.cmp(&other.event_id)
            }
        }
    }
}

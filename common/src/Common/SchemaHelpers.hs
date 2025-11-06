module Common.SchemaHelpers where


import Database.Beam


type Id a = PrimaryKey a Identity

type MaybeId a = PrimaryKey a (Nullable Identity)

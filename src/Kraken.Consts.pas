unit Kraken.Consts;

interface

type
  TKrakenProviderType = (ptPostgres, ptFirebird);

  { Structure Consts }
  const CREATE_SCHEMA       = ' CREATE SCHEMA ';
  const CREATE_TABLE        = ' CREATE TABLE ';
  const ALTER_TABLE         = ' ALTER TABLE ';
  const ADD_COLUMN          = ' ADD COLUMN ';
  const TYPE_COLUMN         = ' TYPE ';
  const DEFAULT             = ' DEFAULT ';
  const NOT_NULL            = ' NOT NULL ';
  const DROP_CONSTRAINT     = ' DROP CONSTRAINT ';
  const ADD_CONSTRAINT      = ' ADD CONSTRAINT ';
  const PRIMARY_KEY         = ' PRIMARY KEY ';

  { Query Consts }
  const INSERT_INTO         = ' INSERT INTO ';
  const DELETE_FROM         = ' DELETE FROM ';
  const UPDATE              = ' UPDATE ';
  const VALUES              = ' VALUES      ';
  const WHERE               = ' WHERE ';
  const ABRE_PARENT         = ' ( ';
  const FECHA_PARENT        = ' ) ';
  const CONFLICT_CONSTRAINT = ' ON CONFLICT  ON CONSTRAINT ';
  const DO_UPDATE           = ' DO UPDATE SET ';
  const _SET                = ' SET ';
  const SEMICOLON           = ' ; ';
  const DOT                 = ' . ';
  const _IN                 = ' IN ';


  {Metadata Consts}
  const TABLE_NAME          = 'TABLE_NAME';
  const SCHEMA_NAME         = 'SCHEMA_NAME';
  const COLUMN_NAME         = 'COLUMN_NAME';
  const COLUMN_TYPENAME     = 'COLUMN_TYPENAME';
  const COLUMN_PRECISION    = 'COLUMN_PRECISION';
  const COLUMN_SCALE        = 'COLUMN_SCALE';
  const COLUMN_LENGTH       = 'COLUMN_LENGTH';
  const CONSTRAINT_NAME     = 'CONSTRAINT_NAME';
  const INDEX_NAME          = 'INDEX_NAME';
  const FKEY_NAME           = 'FKEY_NAME';
  const PKEY_CATALOG_NAME   = 'PKEY_CATALOG_NAME';
  const PKEY_SCHEMA_NAME    = 'PKEY_SCHEMA_NAME';
  const PKEY_TABLE_NAME     = 'PKEY_TABLE_NAME';
  const PKEY_COLUMN_NAME    = 'PKEY_COLUMN_NAME';
  const CATALOG_NAME        = 'CATALOG_NAME';
  const GENERATOR_NAME      = 'GENERATOR_NAME';

  {Column Type Consts}
  const VARCHAR             = ' CHARACTER VARYING ';
  const TIMESTAMP           = ' TIMESTAMP WITHOUT TIME ZONE ';
  const BOOL                = ' BOOLEAN ';
  const NUMERIC             = ' NUMERIC';
  const BPCHAR              = ' CHARACTER ';
  const DATE                = ' DATE ';
  const XML                 = ' XML ';
  const INT                 = ' INTEGER ';
  const TEXTTYPE            = ' TEXT ';
  const BYTEA               = ' BYTEA ';

implementation

end.

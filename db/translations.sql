CREATE TABLE translations (
    id               serial primary key
  , article_id       int not null
  , contributor_id   int not null
  , trans_lang       text not null
  , body             text not null
);

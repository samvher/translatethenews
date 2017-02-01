CREATE TABLE translations (
    id               serial primary key
  , article_id       int not null
  , contributor_id   int not null
  , trans_lang       text not null
  , title            text not null
  , summary          text
  , body             text not null
);

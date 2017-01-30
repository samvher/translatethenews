CREATE TABLE articles (
    id             serial primary key
  , pub_date       date not null
  , title          text not null
  , author         text not null
  , url            text not null
  , summary        text
  , orig_lang      text not null
  , body           text not null
);

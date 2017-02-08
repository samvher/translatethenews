CREATE TABLE articles (
    id                serial primary key
  , contributor_id    int  not null
  , pub_date          text not null
  , title             text not null
  , author            text not null
  , url               text not null
  , summary           text
  , orig_lang         text not null
  , body              text not null
  , av_trans          text not null
  , created           timestamp with time zone not null
  , modified          timestamp with time zone not null
);

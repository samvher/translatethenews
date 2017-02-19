
CREATE TABLE users (
    id          serial primary key
  , name        text not null
  , email       text not null
  , password    text not null
  , read_langs  text not null
  , trans_langs text not null
);


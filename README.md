# translatethenews
Code for translatethenews.org.

## Installation steps
1. Make sure all necessary dependencies are available:
* PostgreSQL
* curl
* zlib
2. In PostgreSQL:
`CREATE ROLE <username> SUPERUSER LOGIN;`
`ALTER USER <username> WITH PASSWORD <password>;`
3. In system:
`createdb <username>`
`psql -U <username> -d <username> -h localhost -a -f db/articles.sql`
`psql -U <username> -d <username> -h localhost -a -f db/translations.sql`
`psql -U <username> -d <username> -h localhost -a -f db/users.sql`
4. Set up `TTN.Hidden` (see `app/TTN/Hidden.hs.example`).
5. Simply `stack install` in this directory and run the exe.


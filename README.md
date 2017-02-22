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
4. Set up `TTN.Hidden` (see `app/TTN/Hidden.hs.example`).
5. Simply `stack install` in this directory and run the exe.


### How to run
1. `git clone https://github.com/Disco-Dave/straw-poll.git`
2. `cd straw-poll`
3. `./build.sh`
4. `docker-compose up -d`
5. `firefox http://localhost:8080`

### Environment Variables
Name                           | Description                               
-------------------------------|------------------------------------------
STRAW_POLL_HTTP_PORT           | Port to run HTTP server on               
STRAW_POLL_PG_USER             | Postgres user for the application       
STRAW_POLL_PG_PASSWORD         | Postgres password for the application  
STRAW_POLL_PG_PORT             | Postgres port for the application     
STRAW_POLL_PG_HOST             | Postgres host for the application    
STRAW_POLL_PG_MIGRATE_USER     | Postgres user to migrate the database
STRAW_POLL_PG_MIGRATE_PASSWORD | Postgres password to migrate the database 

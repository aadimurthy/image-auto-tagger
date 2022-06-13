# image-auto-tagger

DB schema: 

```
use image_auto_tagger;

create table uploaded_images
(id INT  NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
image_name varchar(100) NOT NULL,
image_url  varchar(100) NOT NULL);

create table image_tags
(id INT  NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
langugue varchar(100) NOT NULL,
tag varchar(100) NOT NULL UNIQUE);


create table image_tag_associations
(id INT  NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
image_id INT NOT NULL REFERENCES uploaded_images (id) ON DELETE CASCADE ,
tag_id INT NOT NULL REFERENCES image_tags(id) ON DELETE CASCADE ,
confidence REAL NOT NULL DEFAULT 0 );

```



### How to run
```
./rebar3 shell 
```



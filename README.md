# image-auto-tagger

DB schema: 

```

create table uploaded_images
(id INT  NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
is_detection_enabled BOOLEAN NOT NULL,
image_name varchar(100) NOT NULL,
image_url  varchar(100) NOT NULL);

create table image_tags
(id INT  NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
langugue varchar(100) NOT NULL,
tag varchar(100) NOT NULL);



create table image_tag_associations
(id INT  NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
image_id INT NOT NULL REFERENCES uploaded_images (id) ON DELETE CASCADE ,
tag_id INT NOT NULL REFERENCES image_tags(id) ON DELETE CASCADE ,
confidence REAL NOT NULL DEFAULT 0 );

```



### Prerequisites:
1) Erlang OTP 24 Installation
2) Postgres SQL Backend 


### How to run
```
./rebar3 shell 
```

### APIs Information
#### Post Images
```
Request:

curl --location --request POST 'http://localhost:8888/images' \
--header 'Content-Type: application/json' \
--data-raw '{
    "label": "whale",
    "isDetectionEnabled": true,
    "uri": "https://farm9.staticflickr.com/8505/8441256181_4e98d8bff5_z_d.jpg"
}'

Response:

{
    "label": "whale",
    "isDetectionEnabled": true,
    "uri": "https://farm9.staticflickr.com/8505/8441256181_4e98d8bff5_z_d.jpg"
}

```
#### GET Images/{ImageId}
```
Request:

curl --location --request GET 'http://localhost:8888/images/2'
}'

Response:

{
    "imageLabel": "dog",
    "imageUri": "https://i.imgur.com/OB0y6MR.jpg",
    "isDetectionEnabled": true,
    "tags": {
        "newfoundland": 100.0,
        "dog": 100.0,
        "canine": 100.0,
        "domestic animal": 93.90152740478516,
        "pet": 76.24073028564453,,
      ...............................
   
        "smile": 7.126430988311768
    }
}
```

#### GET Images?objects="dog,whale"
```
Request:

curl --location --request GET 'http://localhost:8888/images?objects="dog,whale"'

Response:

[
    {
        "confidence": 100.0,
        "imageId": 2,
        "imageLable": "dog",
        "imageUri": "https://i.imgur.com/OB0y6MR.jpg",
        "tag": "dog"
    },
    {
        "confidence": 100.0,
        "imageId": 5,
        "imageLable": "whale",
        "imageUri": "https://farm9.staticflickr.com/8505/8441256181_4e98d8bff5_z_d.jpg",
        "tag": "whale"
    },
    {
        "confidence": 100.0,
        "imageId": 6,
        "imageLable": "whale",
        "imageUri": "https://farm9.staticflickr.com/8505/8441256181_4e98d8bff5_z_d.jpg",
        "tag": "whale"
    }
]

```

#### GET Images

```
Request:

curl --location --request GET 'http://localhost:8888/images'

Response:

[
    {
        "imageId": 5,
        "imageLable": "whale",
        "imageUri": "https://farm9.staticflickr.com/8505/8441256181_4e98d8bff5_z_d.jpg",
        "isDetectionEnabled": true,
        "tags": "baleen whale, whale, ......"
    },
    {
        "imageId": 6,
        "imageLable": "whale",
        "imageUri": "https://farm9.staticflickr.com/8505/8441256181_4e98d8bff5_z_d.jpg",
        "isDetectionEnabled": true,
        "tags": "baleen whale, whale, sea,......"
    },
    {
        "imageId": 2,
        "imageLable": "dog",
        "imageUri": "https://i.imgur.com/OB0y6MR.jpg",
        "isDetectionEnabled": true,
        "tags": "newfoundland, dog, canine,...."
    },
    {
        "imageId": 1,
        "imageLable": "cat",
        "imageUri": "https://i.imgur.com/OnwEDW3.jpg",
        "isDetectionEnabled": true,
        "tags": "bridge, structure, ......"
    },
    {
        "imageId": 3,
        "imageLable": "cheetah",
        "imageUri": "https://farm2.staticflickr.com/1533/26541536141_41abe98db3_z_d.jpg",
        "isDetectionEnabled": true,
        "tags": []
    },
    {
        "imageId": 4,
        "imageLable": "bird",
        "imageUri": "https://farm4.staticflickr.com/3075/3168662394_7d7103de7d_z_d.jpg",
        "isDetectionEnabled": true,
        "tags": []
    }
]
```





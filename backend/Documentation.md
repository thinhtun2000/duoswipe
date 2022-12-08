# Backend


## Class

#### User  
    __tablename__ = 'users'
    user_id = db.Column(db.INTEGER, primary_key=True, autoincrement=True)
    name = db.Column(db.String(16), unique=True)
    password = db.Column(db.String(16), nullable=False)
    email = db.Column(db.String(32), nullable=False, unique=True)
    language_id = db.Column(db.INTEGER)
    location_id = db.Column(db.INTEGER)
    pref_pos = db.Column(db.INTEGER)
    pref_lang = db.Column(db.INTEGER)
    pref_day = db.Column(db.String(16))
    pref_time = db.Column(db.String(16))
    pos_1 = db.Column(db.INTEGER)
    pos_2 = db.Column(db.INTEGER)
    rank_id = db.Column(db.INTEGER)
  
>`as_dict(User)`  
> + Return everything as a dictionary of the user.  
>  
>`as_dict_safe(User)`  
> + Return everything except password as a dictionary of the user.  
>  
>`get_id(User)`  
> + Return user_id.  

#### Match
    __tablename__ = 'matches'
    match_id = db.Column(db.INTEGER, primary_key=True, autoincrement=True)
    user_id_1 = db.Column(db.INTEGER)
    user_id_2 = db.Column(db.INTEGER)
    user1_match = db.Column(db.BOOLEAN)
    user2_match = db.Column(db.BOOLEAN)
    match_h = db.Column(db.BOOLEAN) 

#### Language
    __tablename__ = 'language'
    language_id = db.Column(db.INTEGER, primary_key=True)
    name = db.Column(db.VARCHAR(16))

#### Location
    __tablename__ = 'location'
    location_id = db.Column(db.INTEGER, primary_key=True)
    name = db.Column(db.VARCHAR(16))

#### Position
    __tablename__ = 'position'
    position_id = db.Column(db.INTEGER, primary_key=True)
    name = db.Column(db.VARCHAR(16))
    sprite = db.Column(db.VARCHAR(32))

 
## Global Method

#### Create entry
>`create_usercreate_user(name, pwd, email, language_id, location_id, pref_pos, pref_lang,
  pref_day, pref_time, pos_1, pos_2, rank):`  
+ ##### Description
&emsp;&emsp; Create a new user entry and store it into database (Table: users).  

>`create_match(user_id_1, user_id_2, user1_match, user2_match, match_h)` 
+ ##### Description 
&emsp;&emsp; Create a new match entry and store it into database (Table: matches).  

>`create_language(language_id, name)`  
+ ##### Description
&emsp;&emsp; Create a new language entry and store it into database(Table: language).
    
>`create_loc(location_id, name)`  
+ ##### Description
&emsp;&emsp; Create a new location entry and store it into database(Table: location).

>`create_position(position_id, name, sprite)`  
+ ##### Description
&emsp;&emsp; Create a new position entry and store it into database(Table: position).  
  
#### Update Function
>`update_profile(userId, language_id, location_id, pref_pos, pref_lang,
                   pref_day, pref_time, pos_1, pos_2, rank)`
+ ##### Description
&emsp;&emsp; Update user profile

>`update_profile_info(userId, language_id, location_id, pos_1, pos_2, rank)`
+ ##### Description
&emsp;&emsp; Update user information in user profile

>`update_profile_pref(userId, pref_pos, pref_lang, pref_day, pref_time)`
+ ##### Description
&emsp;&emsp; Update user preferences in user profile

#### Delete Function
>`delete_user(userId)`
+ ##### Description
&emsp;&emsp; Delete a specific user from database
+ ##### Parameter
&emsp;&emsp; userId: Integer user_id

#### Compare Function
>`compare(value_1, value_2)`
+ ##### Description
&emsp;&emsp; Compare two values
+ ##### Parameter
&emsp;&emsp; value_1: Integer  
&emsp;&emsp; value_2: Integer
+ ##### Return
&emsp;&emsp; Return 0 if two values are different or missing information.  
&emsp;&emsp; Return 1 if value_1 is equal to value_2

#### Matching Function
>`matching(user: User)`
+ ##### Description
&emsp;&emsp; Matching for the current user
+ ##### Parameter
&emsp;&emsp; user: User object  
+ ##### Return
&emsp;&emsp; Returns all the user_id from the highest priority to the last

#### load_user Function
> `load_user(user_id)`
+ ##### Description
&emsp;&emsp; Load the user by user_id
+ ##### Parameter
&emsp;&emsp; user_id: Integer  
+ ##### Return
&emsp;&emsp; User object


## API

#### Matching Function
> `/`
> - 
> + Description: Register new user
> + Methods: POST
> + Request Parameters: username, password, email
> 
> `/delete/<int:user_id>`
> - 
> + Description: Delete user by user_id
> 
> `/profile/<int:userId>`
> - 
> + Description: Return user profile  
> + Method: GET  
> + Return example:  
{  
  "email": "test1",  
  "language": "English",  
  "location": "Las Cruces",  
  "name": "test1",  
  "pos_1": "Tank",  
  "pos_2": "Bruiser",  
  "pref_day": null,  
  "pref_lang": null,  
  "pref_pos": null,  
  "pref_time": null,  
  "rank": "Iron",  
  "user_id": 1  
}
> 
> `/profile-info/<int:userId>`
> - 
> + Description: Update user information in profile
> + Methods: POST
> + Request Parameters: json object   
{  
  "user_id": 1,  
  "language_id": "English",  
  "location_id": "Las Cruces",  
  "pos_1": "Tank",  
  "pos_2": "ADC",  
  "rank": "Iron"  
}
> + Return example:  
{'status': 'success', 'user_id': 123}
> 
> `/profile-pref/<int:userId>`
> - 
> + Description: Update user preferences in profile
> + Methods: POST
> + Request Parameters: json object   
{  
  "user_id": 1,  
  "pref_pos": "Tank",  
  "pref_lang": "English",  
  "pref_day": "Weekday",  
  "pref_time": "Night"  
}
> + Return example:  
{'status': 'success', 'user_id': 123}
> 
> `/login`
> - 
> + Description: user login
> + Methods: POST
> + Request Parameters: json object  
{'email': 'xxxx@xx@.com', 'password': 123}
> + Return example:
{'status': 'success', 'user_id': 123}
> 
> `/logout`
> - 
> + Description: user logout
> 
> `/user`
> - 
> + Description:
> 
> `/signup`
> - 
> + Description:
> 
> `/match/<int:user_id>`
> - 
> + Description:
> 
> `/matched/<int:user_id>`
> - 
> + Description:
> 
> `/matched_update`
> - 
> + Description:

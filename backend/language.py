from flask import Flask, render_template, url_for, request, redirect
from flask_sqlalchemy import SQLAlchemy

# Connect to Mysql
DIALECT = 'mysql'
DRIVER = 'pymysql'
USERNAME = 'root'
PASSWORD = 'root'
HOST = '127.0.0.1'
PORT = '3306'
DATABASE = 'duoswipe'
URI = '{}+{}://{}:{}@{}:{}/{}?charset=UTF8MB4'.format(
    DIALECT, DRIVER, USERNAME, PASSWORD, HOST, PORT, DATABASE
)

app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = URI
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False

db = SQLAlchemy(app)


# Table 'Language'
class Language(db.Model):
    __tablename__ = 'position'
    language_id = db.Column(db.INTEGER, primary_key=True)
    name = db.Column(db.VARCHAR(16))

    def __repr__(self):
        return 'Language %r' % self.language_id


# Insert into 'language'
def create_language(language_id=None, name=None):
    language = Language()
    language.language_id = language_id
    language.name = name


    db.session.add(language)
    db.session.commit()



if __name__ == "__main__":
    app.run(debug=True)
    # create_user("user1", "user1", "user1")

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


# Table 'champ'
class Champ(db.Model):
    __tablename__ = 'champ'
    champ_id = db.Column(db.INTEGER, primary_key=True)
    name = db.Column(db.VARCHAR(32))
    sprite = db.Column(db.VARCHAR(256))

    def __repr__(self):
        return 'Champ %r' % self.champ_id


# Insert into 'champ'
def create_champ(champ_id=None, name=None, sprite=None):
    champ = Champ()
    champ.champ_id = champ_id
    champ.name = name
    champ.sprite = sprite


    db.session.add(champ)
    db.session.commit()



if __name__ == "__main__":
    app.run(debug=True)
    # create_user("user1", "user1", "user1")

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


# Table 'rank'
class Rank(db.Model):
    __tablename__ = 'rank'
    rank_id = db.Column(db.INTEGER, primary_key=True)
    name = db.Column(db.VARCHAR(16))
    sprite = db.Column(db.VARCHAR(256))

    def __repr__(self):
        return 'Rank %r' % self.rank_id


# Insert into 'rank'
def create_rank(rank_id=None, name=None, sprite=None):
    rank = Rank()
    rank.rank_id = rank_id
    rank.name = name
    rank.sprite = sprite


    db.session.add(rank)
    db.session.commit()



if __name__ == "__main__":
    app.run(debug=True)
    # create_user("user1", "user1", "user1")

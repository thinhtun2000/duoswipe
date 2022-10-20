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


# Table 'matches'
class Match(db.Model):
    __tablename__ = 'matches'
    match_id = db.Column(db.INTEGER, primary_key=True, autoincrement=True)
    user_id_1 = db.Column(db.INTEGER)
    user_id_2 = db.Column(db.INTEGER)
    user1_match = db.Column(db.BOOLEAN)
    user2_match = db.Column(db.BOOLEAN)

    def __repr__(self):
        return 'Match %r' % self.match_id


# Insert into 'match'
def create_match(user_id_1=None, user_id_2=None, user1_match=None, user2_match=None):
    match = Match()
    match.user_id_1 = user_id_1
    match.user_id_2 = user_id_2
    match.user1_match = user1_match
    match.user2_match = user2_match

    db.session.add(match)
    db.session.commit()



if __name__ == "__main__":
    app.run(debug=True)
    # create_user("user1", "user1", "user1")

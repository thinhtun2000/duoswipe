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


# Table 'rating_comment'
class Rate_Comment(db.Model):
    __tablename__ = 'rating_comment'
    unique_id = db.Column(db.INTEGER, primary_key=True, autoincrement=True)
    rating = db.Column(db.INTEGER)
    comment = db.Column(db.VARCHAR(512))
    author = db.Column(db.INTEGER)
    receiver = db.Column(db.INTEGER)

    def __repr__(self):
        return 'Rate_Comment %r' % self.unique_id


# Insert into 'R_C'
def create_r_c(rating=None, comment=None, author=None, receiver=None):
    R_C = Rate_Comment()
    R_C.rating = rating
    R_C.comment = comment
    R_C.author = author
    R_C.receiver = receiver

    db.session.add(R_C)
    db.session.commit()



if __name__ == "__main__":
    app.run(debug=True)
    # create_user("user1", "user1", "user1")

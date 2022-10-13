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


# Table 'user_rank'
class U_R(db.Model):
    __tablename__ = 'user_rank'
    user_id = db.Column(db.INTEGER, nullable=False)
    rift = db.Column(db.INTEGER)
    tft = db.Column(db.INTEGER)


    def __repr__(self):
        return 'U_R %r' % self.user_id


# Insert into 'u_r'
def create_user_rank(user_id=None, rift=None, tft=None):
    u_r = U_R()
    u_r.user_id = user_id
    u_r.rift = rift
    u_r.tft = tft


    db.session.add(u_r)
    db.session.commit()



if __name__ == "__main__":
    app.run(debug=True)
    # create_user("user1", "user1", "user1")

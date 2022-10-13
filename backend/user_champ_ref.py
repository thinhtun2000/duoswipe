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


# Table 'user_champ_ref'
class U_C_R(db.Model):
    __tablename__ = 'user_champ_ref'
    user_id = db.Column(db.INTEGER,  nullable=False)
    champ_id = db.Column(db.INTEGER, nullable=False)



    def __repr__(self):
        return 'U_C_R %r' % self.user_id


# Insert into 'u_c_r'
def create_user_champ_ref(user_id=None, champ_id=None):
    u_c_r = U_C_R()
    u_c_r.user_id = user_id
    u_c_r.champ_id = champ_id


    db.session.add(u_c_r)
    db.session.commit()



if __name__ == "__main__":
    app.run(debug=True)
    # create_user("user1", "user1", "user1")

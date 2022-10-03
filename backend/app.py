

from flask import Flask, render_template, url_for, request, redirect
from flask_sqlalchemy import SQLAlchemy


# Connect to Mysql
DIALECT = 'mysql'
DRIVER = 'pymysql'
USERNAME = 'root'
PASSWORD = ''
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


# Table 'users'
class User(db.Model):
    __tablename__ = 'users'
    user_id = db.Column(db.INTEGER, primary_key=True, autoincrement=True)
    name = db.Column(db.String(16), unique=True)
    password = db.Column(db.String(16), nullable=False)
    email = db.Column(db.String(32), nullable=False, unique=True)
    location_id = db.Column(db.INTEGER)
    pref_pos = db.Column(db.INTEGER)
    pref_lang = db.Column(db.INTEGER)
    pref_day = db.Column(db.String(16))
    pref_time = db.Column(db.String(16))
    pos_1 = db.Column(db.INTEGER)
    pos_2 = db.Column(db.INTEGER)

    def __repr__(self):
        return 'User %r' % self.user_id


# Insert into 'users'
def create_user(name, pwd, email, location_id=None, pref_pos=None,
                pref_day=None, pref_time=None, pos_1=None, pos_2=None):
    user = User()
    user.name = name
    user.password = pwd
    user.email = email

    db.session.add(user)
    db.session.commit()


# Delete user
def delete_user(userId):
    user = User.query.filter_by(user_id=userId).first()
    db.session.delete(user)
    db.session.commit()


@app.route('/', methods=['POST', 'GET'])
# get user information from input
def index():
    if request.method == 'POST':
        name = request.form['name']
        pwd = request.form['password']
        email = request.form['email']

        try:
            create_user(name, pwd, email)
            return redirect('/')
        except:
            return 'There was an issue adding your information'

    else:
        # display user information ordered by user_id
        users = User.query.order_by(User.user_id).all()
        return render_template('index.html', users=users)


@app.route('/delete/<int:user_id>')
# delete user information
def delete(user_id):
    try:
        delete_user(user_id)
        return redirect('/')
    except:
        return 'There was an issue deleting your information'


if __name__ == "__main__":
    app.run(debug=True)
    #create_user("user1", "user1", "user1")
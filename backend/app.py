from flask import Flask, render_template, request, redirect
from flask_sqlalchemy import SQLAlchemy
from flask_cors import CORS
# from flask_login import LoginManager, login_user


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

CORS(app)
db = SQLAlchemy(app)


# create LoginManager object
# login_m = LoginManager()
# login_m.login_view = 'login'
# login_m.login_message = 'Access denied'
# login_m.login_message_category = 'info'

# link login_m to app
# login_m.init_app(app)


# Table 'users'
class User(db.Model):
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

    def __repr__(self):
        return 'User %r' % self.user_id

    def as_dict(self):
        return {c.name: getattr(self, c.name) for c in self.__table__.columns}


# Insert into 'users'
def create_user(name, pwd, email, language_id=None, location_id=None, pref_pos=None, pref_lang=None,
                pref_day=None, pref_time=None, pos_1=None, pos_2=None):
    user = User()
    user.name = name
    user.password = pwd
    user.email = email

    db.session.add(user)
    db.session.commit()


# Update user
def update_profile(userId, language_id=None, location_id=None, pref_pos=None, pref_lang=None,
                   pref_day=None, pref_time=None, pos_1=None, pos_2=None):
    user = User.query.get_or_404(userId)
    user.language_id = language_id
    user.location_id = location_id
    user.pref_pos = pref_pos
    user.pref_lang = pref_lang
    user.pref_day = pref_day
    user.pref_time = pref_time
    user.pos_1 = pos_1
    user.pos_2 = pos_2
    db.session.commit()


# Delete user
def delete_user(userId):
    user = User.query.get_or_404(userId)
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

    elif request.method == 'GET':
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


@app.route('/profile/<int:userId>', methods=['GET', 'POST'])
# get user profile
def get_user(userId):
    if request.method == 'GET':
        try:
            user = User.query.get_or_404(userId)
            return User.as_dict(user)
        except:
            return 'There was an issue getting your information'

    elif request.method == 'POST':
        language_id = request.form['language_id']
        location_id = request.form['location_id']
        pref_pos = request.form['pref_pos']
        pref_lang = request.form['pref_lang']
        pref_day = request.form['pref_day']
        pref_time = request.form['pref_time']
        pos_1 = request.form['pos_1']
        pos_2 = request.form['pos_2']

        try:
            update_profile(userId, language_id, location_id, pref_pos, pref_lang,
                           pref_day, pref_time, pos_1, pos_2)
            return redirect('/profile/' + str(userId))
        except:
            return 'There was an issue adding your information'


# @app.route('/login', methods=['GET', 'POST'])
# def login():
#     if request.method == 'POST':
#         user_id = request.form['user_id']
#         user = User.query.get(user_id)
#
#         if user is not None and request.form['password'] == user['password']:
#             curr_user = User()
#             curr_user.user_id = user_id
#
#             login_user(curr_user)
#
#             return redirect('/')
#
#         else:
#             return "Incorrect username or password"
#
#     # GET
#     return render_template('login.html')


@app.route('/login', methods=['GET', 'POST'])
def login():
    if request.method == 'POST':
        # query user
        user_email = request.form['email']
        user = User.query.filter(User.email == user_email).first()

        # check password
        if user is not None and request.form['password'] == user['password']:
            return 'success'
        else:
            return 'fail'
    # GET
    return render_template('login.html')


if __name__ == "__main__":
    app.run(debug=True)
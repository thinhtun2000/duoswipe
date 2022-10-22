from flask import Flask, render_template, request, redirect
from flask_sqlalchemy import SQLAlchemy
from flask_cors import CORS, cross_origin
from flask_login import LoginManager, login_user, logout_user, login_required

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
app.config['CORS_HEADERS'] = 'Content-Type'

cors = CORS(app, origins=["http://localhost:4200"])

db = SQLAlchemy(app)


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

    def get_id(self):
        return self.user_id

    @property
    def is_authenticated(self):
        return True

    @property
    def is_active(self):
        return True

    @property
    def is_anonymous(self):
        return False


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


# flask-login
login_manager = LoginManager()
login_manager.login_view = 'login'
login_manager.login_message = 'Access denied'
login_manager.login_message_category = 'info'
login_manager.init_app(app)


@login_manager.user_loader
def load_user(user_id):
    user = User.query.filter(User.user_id == user_id).first()
    return user


@app.route('/login', methods=['GET', 'POST'])
@cross_origin(origin='localhost',headers=['Content- Type','Authorization'])
def login():
    if request.method == 'POST':
        # query user
        user_info = request.get_json()
        user_email = user_info['email']
        user = User.query.filter(User.email == user_email).first()
        user = User.as_dict(user)

        # check password
        if user is not None and user_info['password'] == user['password']:
            login_user(user)
            return {'status': 'success', 'user_id': user['user_id']}
        else:
            return 'fail'
    # GET
    return render_template('login.html')


@app.route('/logout', methods=['GET', 'POST'])
@login_required
def logout():
    logout_user()
    return 'Successfully logged out'


@app.route('/user', methods=['GET', 'POST'])
def return_user():
    if request.method == 'GET':
        try:
            users = User.query.order_by(User.user_id).all()

            res = []
            for user in users:
                res.append(User.as_dict(user))
            return res

        except:
            return 'There was an issue getting your information'


@app.route('/signup', methods=['POST', 'GET'])
def signup():
    if request.method == 'POST':
        name = request.form['name']
        pwd = request.form['password']
        email = request.form['email']
        try:
            create_user(name, pwd, email)
            return redirect('/')
        except:
            return 'There was an issue'


# Matching algorithm
def matching(user: User):
    # language_id = db.Column(db.INTEGER)
    # location_id = db.Column(db.INTEGER)
    # pref_pos = db.Column(db.INTEGER)
    # pref_lang = db.Column(db.INTEGER)
    # pref_day = db.Column(db.String(16))
    # pref_time = db.Column(db.String(16))
    # pos_1 = db.Column(db.INTEGER)
    # pos_2 = db.Column(db.INTEGER)

    total_score = 100
    weights = [1, 1, 1, 1, 1, 1, 1]
    current_score = total_score
    other_users = User.query.order_by(User.user_id).all()

    # result will be a list of user_id
    result = []
    return result





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

@app.route('/matched//<int:user_id_1>', methods=['GET', 'POST'])
# matched user
def return_user_matched(user_id_1):
    if request.method == 'GET':
        try:
            if Match.user1_match == True and Match.user2_match == True:
                users = User.query.order_by(Match.user_id_2).all()

        except:
            return 'There was an issue getting your information'








if __name__ == "__main__":
    app.run(debug=True)
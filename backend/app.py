from flask import render_template, request, redirect
from flask_cors import cross_origin
from flask_login import LoginManager, login_user, logout_user, login_required
import sys
from R_Dict import rank_ref, position_ref, location_ref, language_ref, get_key
sys.path.insert(0, '../../duoswipe/backend/Model')
from matches import Match, create_match
# from user_rank import U_R, create_user_rank

# # Connect to Mysql
# DIALECT = 'mysql'
# DRIVER = 'pymysql'
# USERNAME = 'root'
# PASSWORD = 'Crd19991206.'
# HOST = '127.0.0.1'
# PORT = '3306'
# DATABASE = 'duoswipe'
# URI = '{}+{}://{}:{}@{}:{}/{}?charset=UTF8MB4'.format(
#     DIALECT, DRIVER, USERNAME, PASSWORD, HOST, PORT, DATABASE
# )
#
# app = Flask(__name__)
# app.config['SQLALCHEMY_DATABASE_URI'] = URI
# app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
# app.config['CORS_HEADERS'] = 'Content-Type'
#
# cors = CORS(app, origins=["http://localhost:4200"])
#
# db = SQLAlchemy(app)


from connToDB import db, app, cors


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
    rank_id = db.Column(db.INTEGER)

    def __repr__(self):
        return 'User %r' % self.user_id

    def as_dict(self):
        return {c.name: getattr(self, c.name) for c in self.__table__.columns}

    def as_dict_safe(self):
        return {'user_id': self.user_id,
                'name': self.name,
                'email': self.email,
                'language': get_key(language_ref, self.language_id),
                'location': get_key(location_ref, self.location_id),
                'pref_pos': get_key(position_ref, self.pref_pos),
                'pref_lang': get_key(language_ref, self.pref_lang),
                'pref_day': self.pref_day,
                'pref_time': self.pref_time,
                'pos_1': get_key(position_ref, self.pos_1),
                'pos_2': get_key(position_ref, self.pos_2),
                'rank': get_key(rank_ref, self.rank_id)
                }

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
                pref_day=None, pref_time=None, pos_1=None, pos_2=None, rank=None):
    user = User()
    user.name = name
    user.password = pwd
    user.email = email

    db.session.add(user)
    db.session.commit()


# Update user
def update_profile(userId, language_id=None, location_id=None, pref_pos=None, pref_lang=None,
                   pref_day=None, pref_time=None, pos_1=None, pos_2=None, rank=None):

    user = User.query.get_or_404(userId)
    if language_id in language_ref:
        user.language_id = language_ref[language_id]
    if location_id in location_ref:
        user.location_id = location_ref[location_id]
    if pos_1 in position_ref:
        user.pos_1 = position_ref[pos_1]
    if pos_2 in position_ref:
        user.pos_2 = position_ref[pos_2]
    if pref_pos in position_ref:
        user.pref_pos = position_ref[pref_pos]
    if pref_lang in language_ref:
        user.pref_lang = language_ref[pref_lang]

    user.pref_day = pref_day
    user.pref_time = pref_time

    if rank in rank_ref:
        user.rank_id = rank_ref[rank]

    db.session.commit()


def update_profile_info(userId, language_id, location_id, pos_1, pos_2, rank):

    user = User.query.get_or_404(userId)
    if language_id in language_ref:
        user.language_id = language_ref[language_id]
    if location_id in location_ref:
        user.location_id = location_ref[location_id]
    if pos_1 in position_ref:
        user.pos_1 = position_ref[pos_1]
    if pos_2 in position_ref:
        user.pos_2 = position_ref[pos_2]
    if rank in rank_ref:
        user.rank_id = rank_ref[rank]
    db.session.commit()


def update_profile_pref(userId, pref_pos, pref_lang, pref_day, pref_time):
    
    user = User.query.get_or_404(userId)
    if pref_pos in position_ref:
        user.pref_pos = position_ref[pref_pos]
    if pref_lang in language_ref:
        user.pref_lang = language_ref[pref_lang]

    user.pref_day = pref_day
    user.pref_time = pref_time
    db.session.commit()


# Delete user
def delete_user(userId):
    user = User.query.get_or_404(userId)
    db.session.delete(user)
    db.session.commit()


@app.route('/', methods=['POST', 'GET'])
def index():
    if request.method == 'POST':
        username = request.form['username']
        pwd = request.form['password']
        email = request.form['email']
        try:
            create_user(username, pwd, email)
            return redirect('/')
        except:
            return 'There was an issue adding your information'

    elif request.method == 'GET':
        # display user information ordered by user_id
        # users = User.query.order_by(User.user_id).all()
        return render_template('index.html')


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
            print(User.as_dict_safe(user))
            return User.as_dict_safe(user)
        except:
            return 'There was an issue getting your information'

    elif request.method == 'POST':
        updated_user = request.get_json()
        language_id = updated_user['language']
        location_id = updated_user['location']
        pref_pos = updated_user['pref_pos']
        pref_lang = updated_user['pref_lang']
        pref_day = updated_user['pref_day']
        pref_time = updated_user['pref_time']
        pos_1 = updated_user['pos_1']
        pos_2 = updated_user['pos_2']
        rank = updated_user['rank']

        try:
            update_profile(userId, language_id, location_id, pref_pos, pref_lang,
                           pref_day, pref_time, pos_1, pos_2, rank)
            return {'status': 'success', 'user_id': userId}
        except:
            return 'There was an issue adding your information'


@app.route('/profile-info/<int:userId>', methods=['GET', 'POST'])
# get user profile
def update_user_info(userId):
    if request.method == 'POST':
        updated_user = request.get_json()
        language_id = updated_user['language']
        location_id = updated_user['location']
        pos_1 = updated_user['pos_1']
        pos_2 = updated_user['pos_2']
        rank = updated_user['rank']

        try:
            update_profile_info(userId, language_id, location_id, pos_1, pos_2, rank)
            return {'status': 'success', 'user_id': userId}
        except:
            return 'There was an issue adding your information'


@app.route('/profile-pref/<int:userId>', methods=['GET', 'POST'])
# get user profile
def update_user_pref(userId):
    if request.method == 'POST':
        updated_user = request.get_json()
        pref_pos = updated_user['pref_pos']
        pref_lang = updated_user['pref_lang']
        pref_day = updated_user['pref_day']
        pref_time = updated_user['pref_time']

        try:
            update_profile_pref(userId, pref_pos, pref_lang, pref_day, pref_time)
            return {'status': 'success', 'user_id': userId}
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
@cross_origin(origin='localhost', headers=['Content- Type', 'Authorization'])
def login():
    if request.method == 'POST':
        # query user
        user_info = request.get_json()
        user_email = user_info['email']
        user = User.query.filter(User.email == user_email).first()
        # user = User.as_dict(user)

        # check password
        if user is not None and user_info['password'] == user.password:
            login_user(user)
            return {'status': 'success', 'user_id': user.user_id}
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
                res.append(User.as_dict_safe(user))
            return res

        except:
            return 'There was an issue getting your information'


@app.route('/signup', methods=['POST', 'GET'])
def signup():
    if request.method == 'POST':
        input_form = request.get_json()
        name = input_form['username']
        pwd = input_form['password']
        email = input_form['email']
        try:
            create_user(name, pwd, email)
            user = User.query.filter(User.email == email).first()
            return {'status': 'success', 'user_id': user.user_id}
        except:
            return 'There was an issue'


def compare(value_1, value_2):
    if value_1 is None or value_2 is None:
        return 0
    if int(value_1) == int(value_2):
        return 1
    else:
        return 0


# Matching algorithm
def matching(user: User):
    # 0. location  location_id  int
    # 1. position  pref_pos     int
    #              pos_1        int
    #              pos_2        int
    # 2. language  pref_lang    int
    #              language_id  int
    # 3. rank      rank_id      int

    # index -- 0  1  2  3
    weights = [1, 1, 1, 1]
    target_users = User.query.order_by(User.user_id).all()
    result = []  # return a list of user that has scores from highest to lowest

    for curr_target in target_users:
        # do not match with yourself
        if curr_target.user_id == user.user_id:
            continue

        # check if they matched before
        id_1 = int(user.user_id)
        id_2 = int(curr_target.user_id)
        flag = False
        if id_2 < id_1:  # make sure id_1 < id_2
            id_1, id_2 = id_2, id_1
            flag = True

        matched = False
        
        tables = Match.query.filter(Match.user_id_1 == id_1).all()
        for row in tables:
            if row.user_id_2 == id_2:
                if flag:
                    matched = row.user2_match
                else:
                    matched = row.user1_match
        print(matched)
        if matched:
            continue

        # generate a score for every target_user
        score = 0
        score = score + weights[0] * compare(user.location_id, curr_target.location_id)
        score = score + weights[1] * compare(user.pref_pos, curr_target.pos_1)
        score = score + weights[1] * compare(user.pref_pos, curr_target.pos_2)
        score = score + weights[2] * compare(user.pref_lang, curr_target.language_id)
        score = score + weights[3] * compare(user.rank_id, curr_target.rank_id)

        # elements are tuples: (score, user_id)
        result.append((score, curr_target.user_id))

    result = sorted(result, key=lambda x: x[0], reverse=True)  # sort
    result_id = [User.as_dict_safe(load_user(x[1])) for x in result]  # x[1] is user_id

    return result_id


@app.route('/match/<int:user_id>', methods=['POST', 'GET'])
def match(user_id):
    if request.method == 'GET':
        try:
            user = load_user(user_id)
            return_users = matching(user)
            if return_users is None:
                return {'type': 'user_info', 'content': []}
            else:
                return {'type': 'user_info', 'content': return_users}
        except:
            return 'Error'


@app.route('/matched/<int:user_id>', methods=['GET', 'POST'])
# matched user
def return_user_matched(user_id):
    if request.method == 'GET':
        try:
            matches_1 = Match.query.filter(Match.user_id_1 == user_id).all()  # user is user_id_1
            matches_2 = Match.query.filter(Match.user_id_2 == user_id).all()  # user is user_id_2
            results = []
            for M in matches_1:
                if M.user1_match is True and M.user2_match is True:
                    results.append(User.as_dict_safe(load_user(M.user_id_2)))
            for M in matches_2:
                if M.user1_match is True and M.user2_match is True:
                    results.append(User.as_dict_safe(load_user(M.user_id_1)))
            return results
        except:
            return 'There was an issue'


@app.route('/matched_update', methods=['GET', 'POST'])
def matched_update():
    if request.method == 'POST':
        # receive an object {current_user: id, to_match_user: id}
        input_id = request.get_json()
        id_1 = input_id['current_user']
        id_2 = input_id['to_match_user']
        # make sure they are int values
        id_1 = int(id_1)
        id_2 = int(id_2)
        # id_1 must be the smaller integer
        flag_swapped = False  # Record whether they swapped values
        id_2_found = False  # Record if id_2 exist when id_1 = current_user
        if id_1 > id_2:
            temp = id_1
            id_1 = id_2
            id_2 = temp
            flag_swapped = True

        try:
            # check if the table exist
            match_tbs = Match.query.filter(Match.user_id_1 == id_1).all()
            if match_tbs is None:  # no match record
                if flag_swapped:
                    create_match(id_1, id_2, False, True, False)
                else:
                    create_match(id_1, id_2, True, False, False)
                return {'type': 'bool', 'content': False}

            for tb in match_tbs:
                if not flag_swapped:
                    if int(tb.user_id_2) == id_2:
                        id_2_found = True
                        tb.user1_match = True
                        db.session.commit()
                        if tb.user2_match is True:
                            tb.match_h = True
                            db.session.commit()
                            return {'type': 'bool', 'content': True}
                        else:
                            tb.match_h = False
                            db.session.commit()
                            return {'type': 'bool', 'content': False}
                else:  # flag_swapped is True
                    if int(tb.user_id_2) == id_2:
                        tb.user2_match = True  # id_2 is current_user that wanna match with id_1
                        db.session.commit()
                        if tb.user1_match is True:
                            tb.match_h = True
                            db.session.commit()
                            return{'type': 'bool', 'content': True}
                        else:
                            tb.match_h = False
                            db.session.commit()
                            return {'type': 'bool', 'content': False}

            if not id_2_found:  # no match record
                if not flag_swapped:
                    create_match(id_1, id_2, True, False, False)
                else:
                    create_match(id_1, id_2, False, True, False)
                return {'type': 'bool', 'content': False}
        except:
            return 'There was an issue updating your matched table'


if __name__ == "__main__":
    app.secret_key = 'super secret key'
    app.config['SESSION_TYPE'] = 'filesystem'
    app.run(debug=True)

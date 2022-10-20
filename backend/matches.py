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
    user_id_1 = db.Column(db.INTEGER, db.ForeignKey('user_id'))
    user_id_2 = db.Column(db.INTEGER, db.ForeignKey('user_id'))
    user1_status = db.Column(db.BOOLEAN, default=False)
    user2_status = db.Column(db.BOOLEAN, default=False)
    matched_status = db.column(db.BOOLEAN, default=False)

    def __repr__(self):
        return 'Match %r' % self.match_id

# Insert into 'match'
def create_match(u1, u2):
    match = Match()
    match.user_id_1 = u1
    match.user_id_2 = u2
    match.user1_status = True
    db.session.add(match)
    db.session.commit()

# Finds if a match already exists
def find_match(u1, u2):
    match = match.Query.get(u1, u2)
    if match is None:
        create_match(u1, u2)
    else:
        match.user2_status = True
        match.matched_status = True
        db.session.commit()



if __name__ == "__main__":
    app.run(debug=True)
    # create_user("user1", "user1", "user1")

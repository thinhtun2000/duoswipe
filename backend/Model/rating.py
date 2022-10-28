from app import db


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


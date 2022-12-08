from connToDB import db


# Table 'user_champ_ref'
class U_C_R(db.Model):
    __tablename__ = 'user_champ_ref'
    user_id = db.Column(db.INTEGER, nullable=False)
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

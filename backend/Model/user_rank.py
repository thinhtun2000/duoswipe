from flask import Flask, render_template, request
from app import db


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

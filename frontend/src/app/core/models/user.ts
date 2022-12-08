export interface IUser {
  user_id: string;
  name: string;
  password: string;
  email: string;
  language: string;
  location: string;
  pref_pos: string;
  pref_lang: string;
  pref_day: string;
  pref_time: string;
  pos_1: string;
  pos_2: string;
  rank: string;
}
export class User implements IUser {
  user_id: string;
  name: string;
  password: string;
  email: string;
  language: string;
  location: string;
  pref_pos: string;
  pref_lang: string;
  pref_day: string;
  pref_time: string;
  pos_1: string;
  pos_2: string;
  rank: string;

  constructor(init?: any) {
    if (init?.user_id) this.user_id = init.user_id;
    if (init?.name) this.name = init.name;
    if (init?.password) this.password = init.password;
    if (init?.email) this.email = init.email;
    if (init?.language) this.language = init.language;
    if (init?.location) this.location = init.location;
    if (init?.pref_pos) this.pref_pos = init.pref_pos;
    if (init?.pref_lang) this.pref_lang = init.pref_lang;
    if (init?.pref_day) this.pref_day = init.pref_day;
    if (init?.pref_time) this.pref_time = init.pref_time;
    if (init?.pos_1) this.pos_1 = init.pos_1;
    if (init?.pos_2) this.pos_2 = init.pos_2;
    if (init?.rank) this.rank = init.rank;
  }
}

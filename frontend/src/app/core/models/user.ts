export interface IUser {
  user_id: string;
  name: string;
  password: string;
  email: string;
  language_id: string;
  location_id: string;
  pref_pos: string;
  pref_lang: string;
  pref_day: string;
  pref_time: string;
  pos_1: string;
  pos_2: string;
  rank_id: string;
}
export class User implements IUser {
  user_id: string;
  name: string;
  password: string;
  email: string;
  language_id: string;
  location_id: string;
  pref_pos: string;
  pref_lang: string;
  pref_day: string;
  pref_time: string;
  pos_1: string;
  pos_2: string;
  rank_id: string;

  constructor(init?: any) {
    if (init?.user_id) this.user_id = init.user_id;
    if (init?.name) this.name = init.name;
    if (init?.password) this.password = init.password;
    if (init?.email) this.email = init.email;
    if (init?.language_id) this.language_id = init.language_id;
    if (init?.location_id) this.location_id = init.location_id;
    if (init?.pref_pos) this.pref_pos = init.pref_pos;
    if (init?.pref_lang) this.pref_lang = init.pref_lang;
    if (init?.pref_day) this.pref_day = init.pref_day;
    if (init?.pref_time) this.pref_time = init.pref_time;
    if (init?.pos_1) this.pos_1 = init.pos_1;
    if (init?.pos_2) this.pos_2 = init.pos_2;
    if (init?.rank_id) this.rank_id = init.rank_id;
  }
}

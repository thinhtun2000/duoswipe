export interface IUser {
  id: string;
  firstName: string;
  lastName: string;
  userName: string;
}
export class User implements IUser {
  id: string;
  firstName: string;
  lastName: string;
  userName: string;

  constructor(init?: any) {
    if (init?.id) this.id = init.id;
    if (init?.firstName) this.firstName = init.firstName;
    if (init?.lastName) this.lastName = init.lastName;
    if (init?.userName) this.userName = init.userName;
  }
}

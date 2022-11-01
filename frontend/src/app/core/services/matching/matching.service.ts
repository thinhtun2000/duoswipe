import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { environment } from 'src/environments/environment';
import { User } from '../../models/user';
import { UserService } from '../user/user.service';

@Injectable({
  providedIn: 'root',
})
export class MatchingService {
  private MATCHING_API = `${environment.apiBaseURL}matches_update/`;

  public user: User | null;

  constructor(private userSvc: UserService, private http: HttpClient) {
    this.userSvc.user$.subscribe((user) => {
      this.user = user;
      console.log(this.user);
    });
  }

  public update_match(id_1: string, matchingObject: any) {
    return this.http.get(this.MATCHING_API + `${id_1}`, matchingObject);
  }
}

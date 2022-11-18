import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { environment } from 'src/environments/environment';
import { User } from '../../models/user';
import { UserService } from '../user/user.service';

@Injectable({
  providedIn: 'root',
})
export class MatchApiService {
  private MATCHED_API = `${environment.apiBaseURL}matched/`;

  public user: User | null;

  constructor(private userSvc: UserService, private http: HttpClient) {
    this.userSvc.user$.subscribe((user) => {
      this.user = user;
      console.log(this.user);
    });
  }

  public getMatched(): Observable<any> {
    return this.http.get(this.MATCHED_API + `${this.user?.user_id}`);
  }
}

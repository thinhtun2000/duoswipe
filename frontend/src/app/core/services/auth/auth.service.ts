import { HttpResponseBase } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { map, Observable } from 'rxjs';
import { LoginRequest } from '../../models/loginRequest';
import { AuthApiService } from '../auth-api/auth-api.service';
import { UserApiService } from '../user-api/user-api.service';
import { UserService } from '../user/user.service';

@Injectable({
  providedIn: 'root',
})
export class AuthService {
  loginRqt: LoginRequest = {
    email: 'will',
    password: 'user1',
  };

  constructor(private authApi: AuthApiService) {}

  public login(loginRequestObject: LoginRequest): Observable<any> {
    return this.authApi.login(this.loginRqt).pipe(
      map((response: HttpResponseBase) => {
        console.log(response);
      })
    );
  }
}

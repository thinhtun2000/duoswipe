import { HttpResponseBase } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { map, Observable } from 'rxjs';
import { LoginRequest } from '../../models/loginRequest';
import { LoginResponse } from '../../models/loginResponse';
import { AuthApiService } from '../auth-api/auth-api.service';
import { UserApiService } from '../user-api/user-api.service';
import { UserService } from '../user/user.service';

@Injectable({
  providedIn: 'root',
})
export class AuthService {
  constructor(private authApi: AuthApiService) {}

  public login(loginRequestObject: LoginRequest): Observable<LoginResponse> {
    return this.authApi
      .login(loginRequestObject)
      .pipe(map((response: LoginResponse) => response));
  }
}

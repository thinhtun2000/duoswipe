import { HttpResponseBase } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { catchError, map, Observable, of, tap } from 'rxjs';
import { LoginRequest } from '../../models/loginRequest';
import { LoginResponse } from '../../models/loginResponse';
import { AuthApiService } from '../auth-api/auth-api.service';
import { UserApiService } from '../user-api/user-api.service';
import { UserService } from '../user/user.service';

@Injectable({
  providedIn: 'root',
})
export class AuthService {
  constructor(private authApi: AuthApiService, private userSvc: UserService) {}

  public login(loginRequestObject: LoginRequest): Observable<LoginResponse> {
    return this.authApi
      .login(loginRequestObject)
      .pipe(map((response: LoginResponse) => response));
  }

  logout(): Observable<boolean> {
    return of(true).pipe(
      tap(() => {
        this.userSvc.setUser(null);
        //this.tokenSvc.removeToken();
      }),
      catchError(() => of(false))
    );
  }
}

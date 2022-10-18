import { Injectable } from '@angular/core';
import { Router, NavigationEnd } from '@angular/router';
import { BehaviorSubject, Observable, filter, map } from 'rxjs';

@Injectable({
  providedIn: 'root',
})
export class PreviousRouteService {
  private _currentUrl: BehaviorSubject<string> = new BehaviorSubject<string>(
    ''
  );
  private _previousUrl: BehaviorSubject<string> = new BehaviorSubject<string>(
    ''
  );

  get currentUrl(): string {
    return this._currentUrl.value;
  }

  get previousUrl(): string {
    return this._previousUrl.value;
  }

  get currentUrl$(): Observable<string> {
    return this._currentUrl.asObservable();
  }

  get previousUrl$(): Observable<string> {
    return this._previousUrl.asObservable();
  }

  constructor(private router: Router) {
    // this.router.events.subscribe((response) => {
    //   console.log(response);
    // });

    this.router.events
      .pipe(
        filter((event) => event instanceof NavigationEnd),
        map((event) => event as NavigationEnd)
      )
      .subscribe((event: NavigationEnd) => {
        this._previousUrl.next(this.currentUrl);
        this._currentUrl.next(event.urlAfterRedirects);
      });
  }
}

import { Component, OnInit } from '@angular/core';
import { Observable } from 'rxjs';
import { PreviousRouteService } from 'src/app/core/services/previous-route/previous-route.service';

@Component({
  selector: 'app-forbidden',
  templateUrl: './forbidden.component.html',
  styleUrls: ['./forbidden.component.scss'],
})
export class ForbiddenComponent implements OnInit {
  public previousRoute$: Observable<string>;

  constructor(private previousRouteSvc: PreviousRouteService) {}

  ngOnInit(): void {
    this.previousRoute$ = this.previousRouteSvc.previousUrl$;
  }
}

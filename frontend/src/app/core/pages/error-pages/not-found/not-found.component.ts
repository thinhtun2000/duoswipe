import { Component, OnInit } from '@angular/core';
import { Observable } from 'rxjs';
import { PreviousRouteService } from 'src/app/core/services/previous-route/previous-route.service';

@Component({
  selector: 'app-not-found',
  templateUrl: './not-found.component.html',
  styleUrls: ['./not-found.component.scss'],
})
export class NotFoundComponent implements OnInit {
  public previousRoute$: Observable<string>;

  constructor(private previousRouteSvc: PreviousRouteService) {}

  ngOnInit(): void {
    this.previousRoute$ = this.previousRouteSvc.previousUrl$;
  }
}

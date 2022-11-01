import { Component, ElementRef, OnInit } from '@angular/core';
import { Point } from '../../models/point';
import { User } from '../../models/user';
import { MatchingService } from '../../services/matching/matching.service';
import { SwipeService } from '../../services/swipe/swipe.service';

@Component({
  selector: 'app-swipe-page',
  templateUrl: './swipe-page.component.html',
  styleUrls: ['./swipe-page.component.scss'],
})
export class SwipePageComponent implements OnInit {
  public users: User[];
  public index: number = 0;
  public offsetX: number;
  public offsetY: number;
  public static startPoint: any;
  public moved = false;

  constructor(
    private swipeSvc: SwipeService,
    private element: ElementRef,
    private matching: MatchingService
  ) {}

  ngOnInit(): void {
    this.swipeSvc.getUsers().subscribe((response) => {
      this.users = response;
      console.log(response);
    });
  }

  public handleMousePress(event: MouseEvent) {
    SwipePageComponent.startPoint = { x: event.x, y: event.y };
    // Mouse could be up before it ever moves
    document.addEventListener('mouseup', this.handleMouseUp);
    document.addEventListener('mousemove', this.handleMouseMove, true);
  }

  public handleMouseMove(event: MouseEvent) {
    const card = document.getElementById('first')!;
    if (SwipePageComponent.startPoint != null) {
      this.offsetX = event.x - SwipePageComponent.startPoint.x;
      this.offsetY = event.y - SwipePageComponent.startPoint.y;
      const rotate = this.offsetX * 0.1;
      card.style.transform = `translate(${this.offsetX}px, ${this.offsetY}px) rotate(${rotate}deg)`;
    }
  }

  public handleMouseUp(event: MouseEvent) {
    const card = document.getElementById('first')!;
    console.log(event.x);
    if (
      event.x > SwipePageComponent.startPoint.x &&
      event.x - SwipePageComponent.startPoint.x > card.clientWidth * 0.4
    ) {
      console.log('right');
      () => this.handleSwipeRight();
    } else if (
      event.x < SwipePageComponent.startPoint.x &&
      SwipePageComponent.startPoint.x - event.x > card.clientWidth * 0.4
    ) {
      console.log('left');
      () => this.handleSwipeLeft();
    } else {
      SwipePageComponent.startPoint = null;
      document.removeEventListener('mousemove', this.handleMouseMove, true);
      document.getElementById('first')!.style.transform = '';
    }
  }

  public handleSwipeRight(): void {
    console.log('swiped right');
  }

  public handleSwipeLeft(): void {
    console.log('swipe left');
  }
}

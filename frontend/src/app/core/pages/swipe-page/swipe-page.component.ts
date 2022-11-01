import { Component, ElementRef, OnInit } from '@angular/core';
import { Point } from '../../models/point';
import { User } from '../../models/user';
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

  constructor(private swipeSvc: SwipeService, private element: ElementRef) {}

  ngOnInit(): void {
    this.swipeSvc.getUsers().subscribe((response) => {
      this.users = response;
      console.log(response);
    });
  }

  public swipeLeft() {
    const card = document.getElementById('first')!;
    card.animate(
      [
        // keyframes
        { transform: 'translateX(0px) translateY(0px) rotate(0deg)' },
        { transform: 'translateX(-500px) translateY(200px) rotate(-70deg)' },
      ],
      {
        // timing options
        duration: 500,
        iterations: 1,
      }
    );
  }

  public swipeRight() {
    const card = document.getElementById('first')!;
    card.animate(
      [
        // keyframes
        { transform: 'translateX(0px) translateY(0px) rotate(0deg)' },
        { transform: 'translateX(500px) translateY(200px) rotate(70deg)' },
      ],
      {
        // timing options
        duration: 500,
        iterations: 1,
      }
    );
  }

  public handleMousePress(event: MouseEvent) {
    SwipePageComponent.startPoint = { x: event.x, y: event.y };
    document.addEventListener('mouseup', this.handleMouseUp);
    document.addEventListener('mousemove', this.handleMouseMove, true);
    //document.getElementById('first')!.onmousemove = this.handleMouseMove;
  }

  public handleMouseMove(event: MouseEvent) {
    const card = document.getElementById('first')!;
    if (SwipePageComponent.startPoint != null) {
      this.offsetX = event.x - SwipePageComponent.startPoint.x;
      this.offsetY = event.y - SwipePageComponent.startPoint.y;
      if (Math.abs(this.offsetX) > card.clientWidth * 0.7) {
        () => {
          document.getElementById(
            'first'
          )!.style.transform = `translate(0px, 0px) rotate(0deg)`;
        };
      }
      const rotate = this.offsetX * 0.1;
      card.style.transform = `translate(${this.offsetX}px, ${this.offsetY}px) rotate(${rotate}deg)`;
    }
  }

  public handleMouseUp(event: MouseEvent) {
    SwipePageComponent.startPoint = null;
    document.removeEventListener('mousemove', this.handleMouseMove, true);
    //document.getElementById('first')!.onmousemove = null;
    document.getElementById('first')!.style.transform = '';
  }
}

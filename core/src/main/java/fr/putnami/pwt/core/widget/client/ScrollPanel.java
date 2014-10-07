/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.google.gwt.dom.client.DivElement;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Style.Overflow;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.event.logical.shared.ResizeEvent;
import com.google.gwt.event.logical.shared.ResizeHandler;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.widget.client.base.AbstractPanel;

public class ScrollPanel extends AbstractPanel implements CloneableWidget {

	private final ResizeHandler resizeHandler = new ResizeHandler() {

		@Override
		public void onResize(ResizeEvent event) {
			ScrollPanel.this.reset();
		}
	};

	private Integer offsetTop;
	private Integer offsetBottom;

	private Integer offsetLeft;
	private Integer offsetRight;

	public ScrollPanel() {
		super(DivElement.TAG);
	}

	protected ScrollPanel(ScrollPanel source) {
		super(source);
		this.offsetTop = source.offsetTop;
		this.offsetBottom = source.offsetBottom;
		this.offsetLeft = source.offsetLeft;
		this.offsetRight = source.offsetRight;
		this.cloneSourceWidgets(source);
	}

	@Override
	public IsWidget cloneWidget() {
		return new ScrollPanel(this);
	}

	@Override
	protected void onAttach() {
		super.onAttach();
		Scheduler.get().scheduleDeferred(new ScheduledCommand() {

			@Override
			public void execute() {
				Window.addResizeHandler(ScrollPanel.this.resizeHandler);
				ScrollPanel.this.reset();
			}
		});
	}

	@Override
	public void add(IsWidget child) {
		this.append(child);
	}

	public void setOffsetTop(int offsetTop) {
		this.offsetTop = offsetTop;
		if (this.offsetBottom == null) {
			this.offsetBottom = Integer.valueOf(0);
		}
	}

	public void setOffsetBottom(int offsetBottom) {
		this.offsetBottom = offsetBottom;
		if (this.offsetTop == null) {
			this.offsetTop = Integer.valueOf(0);
		}
	}

	public void setOffsetLeft(int offsetLeft) {
		this.offsetLeft = offsetLeft;
		if (this.offsetRight == null) {
			this.offsetRight = Integer.valueOf(0);
		}
	}

	public void setOffsetRight(int offsetRight) {
		this.offsetRight = offsetRight;
		if (this.offsetLeft == null) {
			this.offsetLeft = Integer.valueOf(0);
		}
	}

	private void reset() {
		this.getElement().getStyle().setOverflow(Overflow.AUTO);
		if (this.offsetTop != null && this.offsetBottom != null) {
			this.getElement().getStyle()
				.setHeight(
					Document.get().getClientHeight() - this.offsetTop.intValue()
						- this.offsetBottom.intValue(), Unit.PX);
		}
		if (this.offsetLeft != null && this.offsetRight != null) {
			this.getElement().getStyle().setWidth(
				Document.get().getClientWidth() - this.offsetLeft.intValue() - this.offsetRight.intValue(),
				Unit.PX);
		}
	}
}

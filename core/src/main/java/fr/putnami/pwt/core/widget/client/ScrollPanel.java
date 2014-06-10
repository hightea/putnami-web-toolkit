/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
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
			reset();
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
		cloneSourceWidgets(source);
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
				Window.addResizeHandler(resizeHandler);
				reset();
			}
		});
	}

	@Override
	public void add(IsWidget child) {
		append(child);
	}

	public void setOffsetTop(int offsetTop) {
		this.offsetTop = offsetTop;
		if (offsetBottom == null) {
			offsetBottom = Integer.valueOf(0);
		}
	}

	public void setOffsetBottom(int offsetBottom) {
		this.offsetBottom = offsetBottom;
		if (offsetTop == null) {
			offsetTop = Integer.valueOf(0);
		}
	}

	public void setOffsetLeft(int offsetLeft) {
		this.offsetLeft = offsetLeft;
		if (offsetRight == null) {
			offsetRight = Integer.valueOf(0);
		}
	}

	public void setOffsetRight(int offsetRight) {
		this.offsetRight = offsetRight;
		if (offsetLeft == null) {
			offsetLeft = Integer.valueOf(0);
		}
	}

	private void reset() {
		getElement().getStyle().setOverflow(Overflow.AUTO);
		if (offsetTop != null && offsetBottom != null) {
			getElement().getStyle().setHeight(Document.get().getClientHeight() - offsetTop.intValue() - offsetBottom.intValue(), Unit.PX);
		}
		if (offsetLeft != null && offsetRight != null) {
			getElement().getStyle().setWidth(Document.get().getClientWidth() - offsetLeft.intValue() - offsetRight.intValue(), Unit.PX);
		}
	}
}

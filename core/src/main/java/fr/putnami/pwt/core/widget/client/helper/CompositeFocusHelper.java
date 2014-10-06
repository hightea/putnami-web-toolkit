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
package fr.putnami.pwt.core.widget.client.helper;

import com.google.common.collect.Sets;
import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.EventTarget;
import com.google.gwt.event.dom.client.BlurEvent;
import com.google.gwt.event.dom.client.BlurHandler;
import com.google.gwt.event.dom.client.DomEvent;
import com.google.gwt.event.dom.client.FocusEvent;
import com.google.gwt.event.dom.client.FocusHandler;
import com.google.gwt.event.dom.client.HasAllFocusHandlers;
import com.google.gwt.event.dom.client.HasFocusHandlers;
import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.event.dom.client.KeyDownEvent;
import com.google.gwt.event.dom.client.KeyDownHandler;
import com.google.gwt.event.shared.GwtEvent;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.Event;
import com.google.gwt.user.client.Event.NativePreviewEvent;
import com.google.gwt.user.client.Event.NativePreviewHandler;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;
import com.google.web.bindery.event.shared.HandlerRegistration;

import java.util.Set;

import fr.putnami.pwt.core.widget.client.util.FocusUtils;

public final class CompositeFocusHelper implements HasAllFocusHandlers {

	public static CompositeFocusHelper createFocusHelper(IsWidget containerWidget, HasFocusHandlers... focusContents) {
		assert containerWidget != null : "containerWidget cannot be null";
		assert containerWidget.asWidget() != null : "containerWidget.asWidget() cannot be null";
		return new CompositeFocusHelper(containerWidget.asWidget(), focusContents);
	}

	private final KeyDownHandler keyDownHandler = new KeyDownHandler() {
		@Override
		public void onKeyDown(KeyDownEvent event) {
			if (event.getNativeKeyCode() == KeyCodes.KEY_TAB) {
				Scheduler.get().scheduleDeferred(new ScheduledCommand() {

					@Override
					public void execute() {
						Element activeElement = FocusUtils.getActiveElement();
						if (activeElement != null && !isOrHasChildOfContainerOrPartner(activeElement)) {
							blur();
						}
					}
				});
			}
		}
	};

	private final FocusHandler focusHandler = new FocusHandler() {
		@Override
		public void onFocus(FocusEvent event) {
			focus();
		}
	};

	private final HandlerManager handlerManager;
	private HandlerRegistration previewHandler;

	private final Widget containerWidget;
	private final Set<HasFocusHandlers> hasFocusContents = Sets.newHashSet();
	private final Set<Element> focusPartners = Sets.newHashSet();

	private boolean focused = false;

	private CompositeFocusHelper(Widget containerWidget, HasFocusHandlers... hasFocusContents) {
		this.containerWidget = containerWidget;
		containerWidget.addDomHandler(keyDownHandler, KeyDownEvent.getType());
		if (hasFocusContents != null) {
			for (HasFocusHandlers hasFocus : hasFocusContents) {
				addHasFocusContent(hasFocus);
			}
		}

		this.handlerManager = new HandlerManager(containerWidget);
	}

	public void addHasFocusContent(HasFocusHandlers hasFocusContent) {
		if (hasFocusContents.add(hasFocusContent)) {
			hasFocusContent.addFocusHandler(focusHandler);
		}
	}

	public void addFocusPartner(Element partner) {
		if (!containerWidget.getElement().isOrHasChild(partner)) {
			focusPartners.add(partner);
		}
	}

	private boolean eventTargetsContainerOrPartner(NativePreviewEvent event) {
		Event nativeEvent = Event.as(event.getNativeEvent());
		EventTarget target = nativeEvent.getEventTarget();
		if (Element.is(target)) {
			return isOrHasChildOfContainerOrPartner(Element.as(target));
		}
		return false;
	}

	private boolean isOrHasChildOfContainerOrPartner(Element target) {
		if (containerWidget.getElement().isOrHasChild(target)) {
			return true;
		}
		for (Element elem : focusPartners) {
			if (elem.isOrHasChild(target)) {
				return true;
			}
		}
		return false;
	}

	private void previewNativeEvent(NativePreviewEvent event) {
		if (event.isCanceled() || event.isConsumed()) {
			event.cancel();
			return;
		}

		boolean eventTargetsContainerOrPartner = eventTargetsContainerOrPartner(event);

		int type = event.getTypeInt();
		switch (type) {
		case Event.ONMOUSEDOWN:
		case Event.ONTOUCHSTART:
			if (!eventTargetsContainerOrPartner) {
				blur();
				return;
			}
			break;
		case Event.ONFOCUS:
			// Not used because focus events are not previewed yet
			if (!eventTargetsContainerOrPartner) {
				blur();
			}
			else {
				focus();
			}
			break;
		default:
			break;
		}
	}

	private void updateHandler() {
		if (previewHandler != null) {
			previewHandler.removeHandler();
			previewHandler = null;
		}
		if (focused) {
			previewHandler = Event.addNativePreviewHandler(new NativePreviewHandler() {
				@Override
				public void onPreviewNativeEvent(NativePreviewEvent event) {
					previewNativeEvent(event);
				}
			});
		}
	}

	private void blur() {
		if (focused) {
			focused = false;
			updateHandler();
			DomEvent.fireNativeEvent(Document.get().createBlurEvent(), this);
		}
	}

	private void focus() {
		if (!focused) {
			focused = true;
			updateHandler();
			DomEvent.fireNativeEvent(Document.get().createFocusEvent(), this);
		}
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addBlurHandler(BlurHandler handler) {
		return handlerManager.addHandler(BlurEvent.getType(), handler);
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addFocusHandler(FocusHandler handler) {
		return handlerManager.addHandler(FocusEvent.getType(), handler);
	}

	@Override
	public void fireEvent(GwtEvent<?> event) {
		handlerManager.fireEvent(event);
	}
}

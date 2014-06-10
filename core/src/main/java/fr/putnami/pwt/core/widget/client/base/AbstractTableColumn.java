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
package fr.putnami.pwt.core.widget.client.base;

import java.util.Collection;
import java.util.Collections;

import com.google.common.collect.Lists;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.model.client.base.HasReadonly;
import fr.putnami.pwt.core.widget.client.TableTH;

public abstract class AbstractTableColumn<T> implements HasReadonly, IsWidget, CloneableWidget {

	private Collection<AbstractTableColumnAspect<T>> aspects;

	private Boolean readonly;
	private Integer colspan;

	public AbstractTableColumn() {

	}

	protected AbstractTableColumn(AbstractTableColumn<T> source) {
		this.readonly = source.readonly;
		this.colspan = source.colspan;
	}

	@Override
	public Boolean getReadonly() {
		return readonly;
	}

	@Override
	public void setReadonly(Boolean readonly) {
		this.readonly = readonly;
	}

	public void setColspan(Integer colspan) {
		this.colspan = colspan;
	}

	public Integer getColspan() {
		return colspan;
	}

	public Collection<AbstractTableColumnAspect<T>> getAspects() {
		return aspects == null ? Collections.EMPTY_LIST : Collections.unmodifiableCollection(aspects);
	}

	public <A extends AbstractTableColumnAspect<T>> A getAspect(Class<A> aspectClass) {
		if (aspects != null) {
			for (AbstractTableColumnAspect<T> aspect : aspects) {
				if (aspectClass != null && aspectClass.equals(aspect.getClass())) {
					return (A) aspect;
				}
			}
		}
		return null;
	}

	public void addAspect(AbstractTableColumnAspect<T> aspect) {
		if (aspects == null) {
			aspects = Lists.newArrayList();
		}
		aspects.add(aspect);
	}

	@Override
	public Widget asWidget() {
		throw new UnsupportedOperationException("An AbstractTableColumn cannot be use as a widget. It exists for use in UiBinder Only");
	}

	public abstract AbstractTableCell<T> createBodyCell();

	public abstract TableTH<T> createHeaderCell();
}

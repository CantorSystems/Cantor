
typedef void (*pTForEach)( /*PObject*/void * Current, /*Pointer*/void * Data, bool & Boolean);

template <class T>
class TRedBlackTree
{
	private:
		TRedBlackTree *	Left;
		TRedBlackTree *	Parent;
		TRedBlackTree *	Right;
		bool				Red;
	public:
		TRedBlackTree();
		~TRedBlackTree();
	protected:
		TRedBlackTree *	FLeft(){return Left;};
		TRedBlackTree *	FParent(){return Parent;};
		TRedBlackTree *	FRight(){return Right;};
		bool 			FRed(){return Red;};
		void			FSetRed(bool State){Red = State;};
    	virtual int		Compare(const T Key1, const T Key2);
		TRedBlackTree *	Root()
			{
				TRedBlackTree * ret;
				while (ret->Parent != NULL)
					ret = ret->Parent;
				return ret;
			};
		TRedBlackTree *	DoFind(TRedBlackTree * P, T * pKey, int * pExact) // спорный момент
			{
  				*pExact = Compare(P->KeyData, *pKey);
  				if (*pExact < 0)
  				{
    				if (P->FLeft() != NULL )
      					return DoFind(P->FLeft());
  				}
  				else
					if (*pExact > 0)
  					{
    					if ( P->FRight() != NULL)
      						return DoFind(P->FRight());
					};
  				return P;
			}
		TRedBlackTree *	Find(const T Key, int & Exact)
			{
				T _Key = Key;
				return DoFind(Root(), &_Key, &Exact);
			};
		TRedBlackTree *	Add(TRedBlackTree * Node)
			{
			};
		void	RotateLeft(TRedBlackTree * pNode)
			{
				TRedBlackTree * pTempNode = pNode->FRight();
				pNode->FRight() = pTempNode->FLeft();
				if (pTempNode->FLeft() != NULL)
					pTempNode->FLeft()->FParent() = pNode;
				if (pTempNode != NULL)
					pTempNode->FParent() = pNode->FParent();
				if (pNode->FParent() != NULL)
				{
					if (pNode == pNode->FParent()->FLeft())
						pNode->FParent()->FLeft() = pTempNode;
					else
						pNode->FParent()->FRight() = pTempNode;
				}
				else
					Root() = pTempNode;
				pTempNode->FLeft() = pNode;
				if (pNode != NULL)
					pNode->FParent() = pTempNode;
			};
		void	RotateRight(TRedBlackTree * pNode)
			{
				TRedBlackTree * pTempNode = pNode->FLeft();
				pNode->FLeft() = pTempNode->FRight();
				if (pTempNode->FRight() != NULL)
					pTempNode->FRight()->FParent() = pNode;
				if (pTempNode != NULL)
					pTempNode->FParent() = pNode->FParent();
				if (pNode->FParent() != NULL)
				{
					if (pNode == pNode->FParent()->FRight())
						pNode->FParent()->FRight() = pTempNode;
					else
						pNode->FParent()->FLeft() = pTempNode;
				}
				else
					Root() = pTempNode;
				pTempNode->FRight() = pNode;
				if (pNode != NULL)
					pNode->FParent() = pTempNode;
			};
			void InternalInsert(TRedBlackTree * pNode)
				{
					TRedBlackTree * _Root = Root();
					while (pNode != _Root && pNode->FParent()->FRed())
					{
						if (pNode->FParent() == pNode->FParent()->FParent()->FLeft() )
						{
							TRedBlackTree * pTempNode = pNode->FParent()->FParent()->FRight();
							if (pTempNode->FRed())
							{
								pNode->FParent()->FSetRed(false);
								pTempNode->FSetRed(false);
								pNode->FParent()->FParent()->FSetRed(true);
								pNode = pNode->FParent()->FParent();
							}
							else
							{
								if (pNode == pNode->FParent()->FRight())
									{
										pNode = pNode->FParent();
										RotateLeft(pNode);
									}
								pNode->FParent()->FSetRed(false);
								pNode->FParent()->FParent()->FSetRed(true);
								RotateRight(pNode->FParent()->FParent());
							}
						}
						else
						{
							TRedBlackTree * pTempNode = pNode->FParent()->FParent()->FLeft();
							if (pTempNode->FRed())
							{
								pNode->FParent()->FSetRed(false);
								pTempNode->FSetRed(false);
								pNode->FParent()->FParent()->FSetRed(true);
								pNode = pNode->FParent()->FParent();
							}
							else
							{
								if (pNode == pNode->FParent()->FLeft())
									{
										pNode = pNode->FParent();
										RotateRight(pNode);
									}
								pNode->FParent()->FSetRed(false);
								pNode->FParent()->FParent()->FSetRed(true);
								RotateRight(pNode->FParent()->FParent());
							}
						}
					}
				}
};

int main()
{
	return 0;
}

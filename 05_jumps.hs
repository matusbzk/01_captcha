import Data.List

inputString :: String
inputString = "0\n2\n0\n-1\n0\n-4\n-5\n-5\n-4\n1\n-6\n-10\n-9\n-1\n-1\n1\n-15\n-15\n-13\n1\n-2\n-13\n-6\n-22\n-10\n-15\n-3\n-19\n1\n-26\n-18\n-13\n-15\n-15\n-10\n-4\n0\n-35\n-4\n-37\n-29\n-30\n-38\n-38\n-13\n-36\n-42\n-5\n-28\n-17\n-34\n-41\n0\n-41\n-36\n-46\n-7\n-51\n-49\n-47\n-45\n-30\n-58\n-33\n-22\n-38\n-49\n-37\n-44\n-53\n-18\n-66\n-46\n-47\n-58\n-22\n-34\n-41\n-13\n-41\n-30\n-34\n-15\n-38\n-60\n-61\n-73\n-20\n-62\n-48\n-19\n-40\n-69\n-86\n-75\n-9\n-29\n-2\n-48\n-96\n-46\n-89\n-76\n-34\n-65\n-38\n-69\n-5\n-12\n-54\n-72\n-87\n-23\n-82\n-12\n-24\n-16\n-115\n-83\n-3\n-109\n-72\n-42\n0\n-48\n-9\n-34\n-67\n-83\n-20\n-33\n-76\n-81\n0\n-16\n-106\n-58\n-91\n-102\n-123\n-135\n-85\n-109\n-61\n-70\n-103\n-43\n-104\n-119\n-75\n-129\n-104\n-87\n-95\n-63\n-1\n-118\n-49\n-71\n-34\n-129\n-52\n-103\n-98\n-132\n-119\n-50\n-36\n-35\n-24\n-98\n-139\n-58\n-25\n-93\n-82\n-87\n1\n-14\n-109\n-89\n-25\n-96\n-60\n-79\n-5\n-124\n-62\n-44\n-98\n-119\n-189\n-66\n-121\n-151\n-4\n-14\n-16\n-154\n-39\n-51\n-127\n-13\n-129\n-98\n-28\n-6\n-174\n-169\n-139\n-22\n-4\n-2\n-48\n-62\n-58\n-163\n-169\n-124\n-104\n-205\n-211\n-43\n2\n-135\n-41\n-88\n-208\n-28\n-124\n-172\n-223\n-76\n-98\n-146\n-55\n-209\n-197\n-134\n-93\n2\n-227\n-39\n-235\n-240\n-206\n-70\n-65\n-38\n-175\n-198\n-80\n-10\n-246\n-228\n-23\n-84\n-177\n-81\n-119\n-161\n-246\n-75\n-72\n-243\n-78\n-233\n-50\n-204\n-7\n-206\n-220\n-46\n-249\n-135\n-130\n-143\n-42\n-65\n-52\n-79\n-112\n-147\n-273\n-54\n-88\n-200\n-227\n-24\n-166\n-113\n-189\n-30\n-174\n-55\n-107\n-14\n-144\n-148\n-46\n-263\n-225\n-85\n-14\n0\n-197\n-10\n-6\n-93\n-153\n-302\n-176\n-182\n-251\n-213\n-9\n-221\n-111\n-39\n-134\n-214\n-155\n-321\n-212\n-2\n-207\n-298\n-124\n-28\n-78\n-213\n-194\n-111\n-159\n-171\n-240\n-175\n-99\n-63\n-162\n-115\n-147\n-265\n-153\n-325\n-19\n-134\n-49\n-240\n-322\n-79\n-61\n-66\n-127\n-292\n-282\n-49\n-114\n-89\n-16\n-353\n-181\n-151\n-72\n-290\n-313\n-279\n-351\n-111\n-220\n-172\n-98\n-28\n-223\n-58\n-51\n-194\n-138\n-143\n-308\n-123\n-28\n-347\n-87\n-115\n-295\n-148\n-116\n-108\n-267\n-51\n-346\n-215\n-44\n-379\n-309\n-237\n0\n-212\n-119\n-231\n-140\n-270\n-91\n-146\n-245\n-232\n-119\n-131\n-398\n-264\n-181\n-303\n-186\n-404\n-280\n-412\n-375\n-292\n-251\n-138\n-36\n-18\n-217\n-117\n-56\n-272\n-312\n-160\n-70\n-130\n-16\n-279\n-159\n-6\n-268\n-283\n-259\n-197\n-378\n-24\n-45\n2\n-390\n-50\n-246\n-233\n-294\n-231\n-364\n-316\n-189\n-231\n-74\n-288\n-286\n-25\n-317\n-371\n-434\n-249\n-54\n-151\n-234\n-95\n-158\n-335\n-362\n-28\n-438\n-103\n-173\n-332\n-97\n-444\n-459\n-255\n-295\n-26\n-120\n-2\n-152\n-432\n-191\n-63\n-313\n-465\n-1\n-228\n-468\n-331\n-231\n-123\n-403\n-479\n-441\n-19\n-75\n-264\n-483\n-371\n-277\n-343\n-52\n-160\n-489\n-182\n-338\n-461\n-233\n-459\n-291\n-54\n-61\n-352\n-276\n-206\n-290\n-456\n-81\n-14\n-331\n-385\n-241\n-149\n-421\n-24\n-12\n-297\n-93\n-412\n-478\n0\n-219\n-157\n-328\n-344\n-367\n-343\n-123\n-349\n-441\n-197\n-317\n-165\n-329\n-515\n-74\n-443\n-197\n-75\n-52\n-534\n-330\n-178\n-509\n-199\n-502\n-429\n-362\n-422\n-555\n-183\n-221\n-461\n-338\n-496\n-28\n-507\n-276\n-271\n-511\n-298\n-426\n-144\n-112\n-198\n-496\n-158\n-350\n-326\n-219\n-315\n-394\n-555\n-10\n-422\n-420\n-216\n-386\n-344\n-374\n-567\n-15\n-23\n-434\n-44\n-346\n-110\n-561\n-198\n-505\n-103\n-374\n-107\n-298\n-38\n-26\n-171\n-235\n-324\n-427\n-359\n-130\n-500\n-31\n-221\n-402\n-240\n-283\n-47\n-20\n-422\n-453\n-31\n-470\n-115\n-97\n-120\n-41\n-590\n-437\n-53\n-563\n-440\n-254\n-545\n-256\n-341\n-325\n-417\n-9\n2\n-442\n-370\n-317\n-404\n-498\n-340\n-402\n-506\n-381\n-484\n-582\n-274\n-157\n-325\n-445\n-200\n-56\n-324\n-31\n-448\n-407\n-460\n-84\n-44\n-387\n-515\n-206\n-617\n-322\n-168\n-340\n-553\n-629\n-407\n-344\n-166\n-619\n-313\n-222\n-139\n-199\n-93\n-474\n-246\n-165\n-503\n-636\n-40\n-298\n-629\n-294\n-73\n-438\n-628\n-632\n-464\n-512\n-496\n-683\n-406\n-241\n-41\n-251\n-95\n-264\n-565\n-183\n-256\n-634\n-436\n-660\n-256\n-528\n-405\n-4\n-184\n-513\n-338\n-476\n-393\n-449\n-373\n-585\n-197\n-334\n-165\n-161\n-559\n-424\n-203\n-1\n-234\n-511\n-562\n-234\n-324\n-339\n-422\n-269\n-399\n-249\n-61\n-630\n-648\n-37\n-190\n-196\n-478\n-150\n-264\n-40\n-409\n-600\n-253\n-708\n-130\n-463\n-568\n-292\n-10\n-350\n-280\n-617\n-25\n-218\n-310\n-72\n-484\n-741\n-701\n-284\n-654\n-442\n-679\n-718\n-360\n-488\n-563\n-192\n-282\n-342\n-368\n-95\n-213\n-511\n-767\n-194\n-216\n-574\n-496\n-770\n-145\n-652\n-203\n-26\n-74\n-564\n-533\n-605\n-236\n-183\n-170\n-755\n-98\n-174\n-478\n-476\n-194\n-167\n-439\n-724\n-605\n-364\n-213\n-35\n-67\n-378\n-452\n-59\n-340\n-663\n-762\n-506\n-650\n-223\n-785\n-53\n-32\n-241\n-214\n-274\n-602\n-308\n-182\n-367\n-351\n-327\n-157\n-526\n-424\n-229\n-66\n-669\n-571\n-538\n-240\n-379\n-528\n-667\n-401\n-832\n-524\n-651\n-91\n-102\n-27\n-586\n-128\n-836\n-35\n-653\n-809\n-109\n-70\n-707\n-387\n-351\n-41\n-7\n-149\n-10\n-614\n-181\n-560\n-24\n-257\n-305\n-303\n-91\n-848\n-249\n-401\n-624\n-265\n-751\n-752\n-367\n-554\n-715\n-419\n-449\n-570\n-62\n-568\n-203\n-341\n-751\n-657\n-347\n-751\n-639\n-742\n-307\n-861\n-706\n-487\n-644\n-612\n-390\n-474\n-565\n-174\n-263\n-377\n-307\n-383\n-390\n-484\n-722\n-806\n-874\n-247\n-570\n-221\n-51\n-215\n-641\n-534\n-427\n-277\n-647\n-912\n-787\n-834\n-270\n-607\n-354\n-593\n-740\n-25\n-222\n-500\n-494\n-940\n-442\n-592\n-938\n-904\n-580\n-20\n-938\n-671\n-199\n-677\n-917\n-903\n-206\n-411\n-917\n-424\n-300\n-889\n-501\n-100\n-117\n-315\n-678\n-664\n-579\n-749\n-636\n-949\n-642\n-968\n-343\n-628\n-190\n-700\n-705\n-339\n-240\n-216\n-628\n-917\n-724\n-481\n-900\n-74\n-291\n-234\n-934\n-642\n-874\n-594\n-955\n-951\n-341\n-463\n-706\n-735\n-556\n-681\n-985\n-285\n-604\n-44\n-153\n-14\n-78\n-958\n-44\n-338\n-765\n-787\n-487\n-441\n-518\n-772\n-632\n-70\n-74\n-630\n-362\n-533\n-684\n-328\n-407\n-193\n-727\n-230\n-454\n-141\n-568\n-802\n-326\n-725\n-464\n-880\n-990\n-34\n"

-- |Input as a list of ints
input :: [Int]
input = map read . lines $ inputString

-- |Stores current position and a list of instructions
type State = (Int, [Int])

-- |From what state do I begin
startState :: State
startState = (0,input)

-- |Checks whether given state is valid (returns false if program reached the exit)
validState :: State -> Bool
validState (pos,jumps) = pos >= 0 && pos < length jumps

-- |Takes a state and performs one instruction
makeJump :: State -> State
makeJump (position, jumps) = (position + dif, newJumps)
            where dif = jumps !! position
                  newJumps = take position jumps ++ (dif+1) : drop (position + 1) jumps

-- |How many steps does it take to reach the exit - result to part 1
-- |Not very effective, takes me like 6 seconds
stepsUntilExit :: Int
stepsUntilExit = fst $ stepsUntilExit' (0,startState)

-- |Iterates a makeJump function, with checking if state is valid
stepsUntilExit' :: (Int, State) -> (Int, State)
stepsUntilExit' (n,state) = if validState state then stepsUntilExit' (n+1, makeJump state) 
                                                else (n,state)

-- |Takes a state and performs one instruction - version for part 2
makeJump2 :: State -> State
makeJump2 (position, jumps) = (position + dif, newJumps)
            where dif = jumps !! position
                  newDif = if dif >= 3 then dif - 1 else dif + 1 
                  newJumps = take position jumps ++ [newDif] ++ drop (position + 1) jumps

-- |How many steps does it take to reach the exit - result to part 2
-- |Actually inefficient af, I have calculated the result in C#
-- |Takes more than 10 minutes
stepsUntilExit2 :: Int
stepsUntilExit2 = fst $ stepsUntilExit2' (0,startState)

-- |Iterates a makeJump2 function, with checking if state is valid
stepsUntilExit2' :: (Int, State) -> (Int, State)
stepsUntilExit2' (n,state) = if validState state then stepsUntilExit2' (n+1, makeJump2 state) 
                                                 else (n,state)
